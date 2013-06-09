module Swensen.NL.Compilation
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

module CM = CompilerMessages
module SA = SemanticAnalysis

let DefaultOffset = (1,1,0)

//todo: currently we assume no optimization

///absOffset is 0 based absolution starting position of code fragment, lineNum is line number starting at 1, colNum is column number starting at 1.
let lexParseAndSemantWith env (lineNum, colNum, absOffset) code =
    let lexbuf = LexBuffer<char>.FromString(code)
    lexbuf.EndPos <- 
        { 
            pos_bol = absOffset-(colNum-1)
            pos_fname=""
            pos_cnum=absOffset
            pos_lnum=lineNum 
        }

    try 
        Parser.start Lexer.tokenize lexbuf
        |> SA.semantWith env
    with
    | CompilerInterruptException ->
        ILTopLevel.Error
    //fslex/yacc do not use specific exception types
    | e when e.Message = "parse error" -> //we handle lex errors explicitly now (this error message should not be possible): || e.Message = "unrecognized input" ->
        CM.Log (CompilerMessage(PositionRange(lexbuf.StartPos,lexbuf.EndPos), MessageType.Syntactic, MessageLevel.Error, -1, e.Message, null)) //todo: we want the real StackTrace
        ILTopLevel.Error
    | e ->
        CM.Log (CompilerMessage(PositionRange(lexbuf.StartPos,lexbuf.EndPos), MessageType.Internal, MessageLevel.Error, -1, e.ToString(), null))  //todo: we want the real StackTrace
        ILTopLevel.Error

let lexParseAndSemant = lexParseAndSemantWith SemanticEnvironment.Default DefaultOffset

//todo: currently this function compilies an expression to a single method
//      set as the entry point of an assembly, obviously this needs to evolve.
///ast -> assemblyName -> unit
let compileFromAil ail asmName =
    let asmFileName = asmName + ".exe"
    let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndSave)
    let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".netmodule", asmFileName, false) //need to specify asmFileName here!
    let tyBuilder = modBuilder.DefineType(asmName + ".Application", TypeAttributes.Public)
    let methBuilder = tyBuilder.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Static, typeof<System.Void>, null)
    
    let ilExpr =
        match ail with
        | ILTopLevel.Expr(x) -> x
        | ILTopLevel.StmtList([ILStmt.Do(x)]) -> x
        | _ -> 
            failwithf "not a valid compiler expression: %A" ail //todo: remove

    let il = methBuilder.GetILGenerator() |> SmartILGenerator.fromILGenerator
    Emission.emit false il ilExpr
    il.Emit(OpCodes.Ret)

    tyBuilder.CreateType() |> ignore
    asmBuilder.SetEntryPoint(methBuilder, PEFileKinds.ConsoleApplication)
    asmBuilder.Save(asmFileName)

///Compile the given source code string into an assembly
///code -> assemblyName -> unit
let compileFromString = 
    lexParseAndSemant>>compileFromAil

///Compile all the given source code files into a single assembly
///fileNames -> assemblyName -> unit
let compileFromFiles fileNames =
    fileNames
    |> Seq.map System.IO.File.ReadAllText                     
    |> String.concat System.Environment.NewLine
    //|> (fun text -> printfn "%s" text; text)
    |> compileFromString