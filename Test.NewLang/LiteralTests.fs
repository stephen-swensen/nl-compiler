module Tests.LiteralTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic
module C = Compilation

[<Fact>]
let ``literal System.Int32 exp`` () =
    test <@ C.eval "3" = 3 @>

[<Fact>]
let ``literal System.Double exp`` () =
    test <@ C.eval "3.0" = 3.0 @>

[<Fact>]
let ``literal System.String exp`` () =
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
    raises<SemanticErrorException> <@ C.eval "null[int32]" @>

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
let ``resolve simple fully qualified generic signature in null expression`` () =
    test <@ C.eval "null[system.collections.generic.list[system.int32]]" = null @>

[<Fact>]
let ``literal nop`` () =
    test <@ C.eval "()" = null @>