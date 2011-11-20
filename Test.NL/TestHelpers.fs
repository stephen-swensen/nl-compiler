namespace Tests
open Swensen.NL

[<AutoOpen>]
module Helpers =
    ///code fragment to reference this assembly within tests so we can test with types defined in this assembly
    let openAsm = sprintf "open \"%s\" in " (System.Reflection.Assembly.GetExecutingAssembly().Location)
    ///code fragment to open the root namespace of this assembly for testing with types defined within this assembly
    let openNamespace = "open Tests in "
    ///prefix used to reference this assembly and this namespace in dynamic NL tests
    let openPrefix = openAsm + openNamespace

    let expectedErrors codes = 
        fun (e:EvaluationException) ->
            let errors = e.Errors
            <@ errors |> Array.map (fun err -> err.Code) = codes @>

type Test1() =
    member __.DoIt1<'a>() = Unchecked.defaultof<'a>
    member __.DoIt2<'a>(x:'a) = x

type Test2<'a> =
    static member DoIt1<'b>() = Unchecked.defaultof<'b>

