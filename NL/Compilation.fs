module Swensen.NL.Compilation
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

module CM = CompilerMessages
module SA = SemanticAnalysis

//todo: currently this function compiles an expression to a single method
//      set as the entry point of an assembly with no optimization options... obviously this needs to evolve.
///ast -> assemblyName -> unit
let compileFromAil (ail:ILExpr) (asmName:string) =
    ()

    let asmFileName = asmName + ".exe"
    let asmBuilder = PersistedAssemblyBuilder(AssemblyName(asmName), typeof<obj>.Assembly);
    let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".netmodule")
    let tyBuilder = modBuilder.DefineType(asmName + ".Application", TypeAttributes.Public)
    let methBuilder = tyBuilder.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Static, typeof<System.Void>, null)

    let ilExpr = ail

    let il = methBuilder.GetILGenerator() |> SmartILGenerator.fromILGenerator
    Emission.emit false il ilExpr
    il.Emit(OpCodes.Ret)

    let ty = tyBuilder.CreateType()

    let metadataBuilder, ilStream, fieldData =
        let mutable ilStreamOut = BlobBuilder()
        let mutable fieldDataOut = BlobBuilder()
        let mb = asmBuilder.GenerateMetadata(&ilStreamOut, &fieldDataOut)
        mb, ilStreamOut, fieldDataOut

    let peBuilder =
        ManagedPEBuilder(
            header = PEHeaderBuilder.CreateExecutableHeader(),
            metadataRootBuilder = new MetadataRootBuilder(metadataBuilder),
            ilStream = ilStream,
            mappedFieldData = fieldData,
            entryPoint = MetadataTokens.MethodDefinitionHandle(methBuilder.MetadataToken))

    let peBlob = BlobBuilder()
    peBuilder.Serialize(peBlob) |> ignore

    use fileStream = new FileStream(asmFileName, FileMode.Create, FileAccess.Write)
    peBlob.WriteContentTo(fileStream)

    let runtimeConfigContent = """{
        "runtimeOptions": {
            "tfm": "net9.0",
            "framework": {
                "name": "Microsoft.NETCore.App",
                "version": "9.0.0"
            }
        }
    }"""
    File.WriteAllText(asmName + ".runtimeconfig.json", runtimeConfigContent)

///Compile the given source code string into an assembly
///code -> assemblyName -> unit
let compileFromString =
    FrontEnd.lexParseAndSemantExpr>>compileFromAil

///Compile all the given source code files into a single assembly
///fileNames -> assemblyName -> unit
let compileFromFiles fileNames =
    fileNames
    |> Seq.map File.ReadAllText
    |> String.concat Environment.NewLine
    //|> (fun text -> printfn "%s" text; text)
    |> compileFromString
