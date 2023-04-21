module Swensen.NL.Compilation
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open FSharp.Text.Lexing
open Lexer
open Parser

module CM = CompilerMessages
module SA = SemanticAnalysis

//todo: currently this function compiles an expression to a single method
//      set as the entry point of an assembly with no optimization options... obviously this needs to evolve.
///ast -> assemblyName -> unit
let compileFromAil (ail:ILExpr) (asmName:string) =
    ()
    //TODO: need to wait for .net 8 for Save support - https://github.com/dotnet/runtime/issues/62956

    //let asmFileName = asmName + ".exe"
    //let asmBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndSave)
    //let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".netmodule", asmFileName, false) //need to specify asmFileName here!
    //let tyBuilder = modBuilder.DefineType(asmName + ".Application", TypeAttributes.Public)
    //let methBuilder = tyBuilder.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Static, typeof<System.Void>, null)

    //let ilExpr = ail

    //let il = methBuilder.GetILGenerator() |> SmartILGenerator.fromILGenerator
    //Emission.emit false il ilExpr
    //il.Emit(OpCodes.Ret)

    //tyBuilder.CreateType() |> ignore
    //asmBuilder.SetEntryPoint(methBuilder, PEFileKinds.ConsoleApplication)
    //asmBuilder.Save(asmFileName)

///Compile the given source code string into an assembly
///code -> assemblyName -> unit
let compileFromString =
    FrontEnd.lexParseAndSemantExpr>>compileFromAil

///Compile all the given source code files into a single assembly
///fileNames -> assemblyName -> unit
let compileFromFiles fileNames =
    fileNames
    |> Seq.map System.IO.File.ReadAllText
    |> String.concat System.Environment.NewLine
    //|> (fun text -> printfn "%s" text; text)
    |> compileFromString