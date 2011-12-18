module Swensen.NL.Compilation
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

type EL = ErrorLogger

///parse from the lexbuf with the given semantic environment
let parseWith env lexbuf =
    try 
        Parser.start Lexer.tokenize lexbuf 
        |> SemanticAnalysis.tycheckWith env
    with
    | CompilerInterruptException ->
        ILTopLevel.Error
    //fslex/yacc do not use specific exception types
    | e when e.Message = "parse error" || e.Message = "unrecognized input" ->
        EL.ActiveLogger.Log
            (CompilerError(PositionRange(lexbuf.StartPos,lexbuf.EndPos), ErrorType.Syntactic, ErrorLevel.Error, -1, e.Message, null)) //todo: we want the real StackTrace
        ILTopLevel.Error
    | e ->
        EL.ActiveLogger.Log
            (CompilerError(PositionRange(lexbuf.StartPos,lexbuf.EndPos), ErrorType.Internal, ErrorLevel.Error, -1, e.ToString(), null))  //todo: we want the real StackTrace
        ILTopLevel.Error

///parse from the string with the given semantic environment
let parseFromStringWith env code =
    let lexbuf = 
        let lexbuf = LexBuffer<char>.FromString(code)
        lexbuf.EndPos <- { pos_bol = 0
                           pos_fname=""
                           pos_cnum=1
                           pos_lnum=1 }
        lexbuf

    parseWith env lexbuf

///parseFromString with the "default" environment
let parseFromString = parseFromStringWith SemanticEnvironment.Default



//should have "tryEval" which doesn't throw?

///Evaluate an NL code string using the default environment.
///If one or more compiler errors occur, then an EvaluationException is throw which contains the list of errors. Warnings are ignored.
let eval<'a> code : 'a = 
    ///Create a dynamic method from a typed expression using the default environment
    let mkDm (ilExpr:ILExpr) =
        let dm = DynamicMethod("Eval", ilExpr.Type, null)
        let il = dm.GetILGenerator()
        Emission.emit il ilExpr
        il.Emit(OpCodes.Ret)
        dm

    EL.InstallDefaultLogger() //may want to switch back to previous logger when exiting eval

    let ilTopLevel = parseFromString code 
    match EL.ActiveLogger.ErrorCount with
    | 0 ->
        let ilExpr =
            match ilTopLevel with
            | ILTopLevel.Exp(x) -> x
            | ILTopLevel.StmtList([ILStmt.Do(x)]) -> x
            | _ -> 
                raise (EvaluationException("not a valid eval expression", [||]))

        let dm = mkDm ilExpr
        dm.Invoke(null,null) |> unbox
    | _ ->
        raise (EvaluationException("compiler errors", EL.ActiveLogger.Errors |> Seq.toArray))

//TODO: the following methods are used for nlc.exe and nli.exe. in the future we will have 
//specialized methodss for the Compiler service, vs. nlc.exe, vs. nli.exe with APPRORPIATE ERRORLOGGERS installed.

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
        | ILTopLevel.Exp(x) -> x
        | ILTopLevel.StmtList([ILStmt.Do(x)]) -> x
        | _ -> 
            failwithf "not a valid compiler expression: %A" ail //todo: remove

    let il = methBuilder.GetILGenerator()
    Emission.emit il ilExpr
    il.Emit(OpCodes.Ret)

    tyBuilder.CreateType() |> ignore
    asmBuilder.SetEntryPoint(methBuilder, PEFileKinds.ConsoleApplication)
    asmBuilder.Save(asmFileName)

///Compile the given source code string into an assembly
///code -> assemblyName -> unit
let compileFromString = 
    parseFromString>>compileFromAil

///Compile all the given source code files into a single assembly
///fileNames -> assemblyName -> unit
let compileFromFiles fileNames =
    fileNames
    |> Seq.map System.IO.File.ReadAllText
    |> String.concat System.Environment.NewLine
    //|> (fun text -> printfn "%s" text; text)
    |> compileFromString

//type NliState =
//    {
//        Assemblies: Assembly list
//        Namespaces: string list
//        Variables: (string,(obj*Type)) Map
//    }
//    with
//        static member Empty = {Assemblies = []; Namespaces=[]; Variables=Map.empty}
//        static member Default =
//            let se = Semant.SemanticEnvironment.Default
//            { NliState.Empty with Assemblies=se.Assemblies; Namespaces=se.Namespaces }
//
/////The NL interactive
//type Nli() = 
//    ///head of list is most recent, this should not be externally mutable
//    let mutable historicalState: NliState list = []
//    ///the current state, this can be externally mutable if so desired
//    let mutable currentState: NliState = NliState.Default
//
//    ///evaluate the code string in the Nli environment
//    let eval code =
//        //WE NEED THE RAW AST FIRST
//        let lexbuf = 
//            let lexbuf = LexBuffer<char>.FromString(code)
//            lexbuf.EndPos <- { pos_bol = 0
//                               pos_fname=""
//                               pos_cnum=1
//                               pos_lnum=1 }
//            lexbuf
//
//        let ast = parseFromStringWith { Semant.SemanticEnvironment.Empty with Assemblies=currentState.Assemblies; Namespaces=currentState.Namespaces; Variables=currentState.Variables |> Map.map (fun _ (_,ty)->ty) } code
//        let dm = System.Reflection.Emit.DynamicMethod("Eval", ast.Type, null)
//            let il = dm.GetILGenerator()
//            emitOpCodes il (Return(ast, ast.Type))
//            dm
//
//        for a in currentState.Assemblies do