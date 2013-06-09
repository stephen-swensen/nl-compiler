[<AutoOpen>]
module Tests.Prelude

open Swensen.NL

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

//let expectedMessages codes = 
//    let messages = MessageLogger.ActiveLogger.GetMessages()
//    <@ messages |> Array.map (fun msg -> msg.Code) = codes @>

