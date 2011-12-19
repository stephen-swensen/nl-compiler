module Tests.LiteralTests

open Xunit
open Xunit.Extensions

open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
module C = Compilation

[<Theory;EvalData>]
let ``literal Int32 exp`` options =
    test <@ C.evalWith options "3" = 3 @>

[<Fact>]
let ``literal Int32 out of range`` () =
    raisesWith 
        <@ C.eval "99999999999999999999999" @>
        (expectedErrors [|26|])

[<Fact>]
let ``literal Double exp`` () =
    test <@ C.eval "3.0" = 3.0 @>

[<Fact>]
let ``literal Double out of range`` () =
    raisesWith 
        <@ C.eval "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999.0" @>
        (expectedErrors [|27|])

[<Fact>]
let ``literal String exp`` () =
    test <@ C.eval "\"hello world\"" = "hello world" @>

[<Fact>]
let ``char literal`` () =
    test <@ C.eval "'c'" = 'c' @>

[<Fact>]
let ``literal true`` () =
    test <@ C.eval "true" = true @>

[<Fact>]
let ``literal false`` () =
    test <@ C.eval "false" = false @>

[<Fact>]
let ``null literal`` () =
    test <@ C.eval "null[string]" = (null:string) @>

[<Fact>]
let ``null literal of value type is invalid`` () =
    raisesWith
        <@ C.eval "null[int32]" @>
        (expectedErrors [|12|])

[<Fact>]
let ``literal typeof value type`` () =
    test <@ C.eval "type[int32]" = typeof<int> @>

[<Fact>]
let ``literal typeof ref type`` () =
    test <@ C.eval "type[string]" = typeof<string> @>

[<Fact>]
let ``literal typeof generic type`` () =
    test <@ C.eval "type[dictionary[string,int32]]" = typeof<Dictionary<string,int>> @>

[<Fact>]
let ``literal typeof could not resolve type`` () =
    raisesWith 
        <@ C.eval<obj> "type[INVALID_TYPE]" @>
        (expectedErrors [|1|])

[<Fact>]
let ``resolve simple fully qualified generic signature in null expression`` () =
    test <@ C.eval "null[system.collections.generic.list[system.int32]]" = null @>

[<Fact>]
let ``literal nop`` () =
    test <@ C.eval "()" = null @>

[<Fact>]
let ``literal default value type`` () =
    test <@ C.eval "default[int32]" = 0 @>

[<Fact>]
let ``literal default ref type`` () =
    test <@ C.eval "default[string]" = null @>

[<Fact>]
let ``literal default void is invalid`` () =
    raisesWith 
        <@ C.eval "default[system.void]" @>
        (expectedErrors [|14|])