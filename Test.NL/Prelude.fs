[<AutoOpen>]
module Tests.Prelude

open Swensen.NL
open System
open System.Reflection
open System.Reflection.Emit

let private csHelpersAssmLocation = typeof<Tests.NonGenericClass1>.Assembly.Location

///code fragment to reference this assembly within tests so we can test with types defined in this assembly
let openAsm = sprintf "open @\"%s\" in " csHelpersAssmLocation
///code fragment to open the root namespace of this assembly for testing with types defined within this assembly
let OpenNamespaceOrType = "open Tests in "
///prefix used to reference this assembly and this namespace in dynamic NL tests
let openPrefix = openAsm + OpenNamespaceOrType

let expectedErrors codes = 
    fun (e:CompilerServiceException) ->
        let errors = e.Errors |> Seq.toArray
        <@ errors |> Array.map (fun err -> err.Code) = codes @>

let expectedWarnings codes (actualWarnings:CompilerMessage seq) = 
    let actualWarnings = actualWarnings |> Seq.toArray
    <@ actualWarnings |> Array.map (fun msg -> msg.Code) = codes @>

let mkTestModBuilder =
    let cnt = ref 0
    fun () ->
        cnt := !cnt + 1
        let asmName = sprintf "TEST-ASSEMBLY-%i" !cnt
        let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndCollect)
        let modBuilder = asmBuilder.DefineDynamicModule(asmName)
        modBuilder

let nextTopLevelTypeName = 
    let count = ref -1
    fun () -> count := !count+1; "TOP_LEVEL" + (!count).ToString()

let nextItName = 
    let count = ref -1
    fun () -> count := !count+1; "it" + (!count).ToString()

//let lexParseAndSemant code = FrontEnd.lexParseAndSemantStmts code (mkTestModBuilder()) nextTopLevelTypeName nextItName
//let expectedMessages codes = 
//    let messages = MessageLogger.ActiveLogger.GetMessages()
//    <@ messages |> Array.map (fun msg -> msg.Code) = codes @>

