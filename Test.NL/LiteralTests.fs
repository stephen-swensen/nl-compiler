module Tests.LiteralTests

open Xunit;; open Xunit.Extensions

open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``literal Int32 exp`` options =
    test <@ evalWith options "3" = 3 @>

[<Theory;EvalData>]
let ``literal Int32 out of range`` options =
    raisesWith 
        <@ evalWith options "99999999999999999999999" @>
        (expectedErrors [|26|])

[<Theory;EvalData>]
let ``literal Double exp`` options =
    test <@ evalWith options "3.0" = 3.0 @>

[<Theory;EvalData>]
let ``literal Double out of range`` options =
    raisesWith 
        <@ evalWith options "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999.0" @>
        (expectedErrors [|27|])

[<Theory;EvalData>]
let ``literal String exp`` options =
    test <@ evalWith options "\"hello world\"" = "hello world" @>

[<Theory;EvalData>]
let ``char literal`` options =
    test <@ evalWith options "'c'" = 'c' @>

[<Theory;EvalData>]
let ``literal true`` options =
    test <@ evalWith options "true" = true @>

[<Theory;EvalData>]
let ``literal false`` options =
    test <@ evalWith options "false" = false @>

[<Theory;EvalData>]
let ``null literal`` options =
    test <@ evalWith options "null[string]" = (null:string) @>

[<Theory;EvalData>]
let ``null literal of value type is invalid`` options =
    raisesWith
        <@ evalWith options "null[int32]" @>
        (expectedErrors [|12|])

[<Theory;EvalData>]
let ``literal typeof value type`` options =
    test <@ evalWith options "type[int32]" = typeof<int> @>

[<Theory;EvalData>]
let ``literal typeof ref type`` options =
    test <@ evalWith options "type[string]" = typeof<string> @>

[<Theory;EvalData>]
let ``literal typeof generic type`` options =
    test <@ evalWith options "type[dictionary[string,int32]]" = typeof<Dictionary<string,int>> @>

[<Theory;EvalData>]
let ``literal typeof could not resolve type`` options =
    raisesWith 
        <@ evalWith<obj> options "type[INVALID_TYPE]" @>
        (expectedErrors [|1|])

[<Theory;EvalData>]
let ``resolve simple fully qualified generic signature in null expression`` options =
    test <@ evalWith options "null[system.collections.generic.list[system.int32]]" = null @>

[<Theory;EvalData>]
let ``literal nop`` options =
    test <@ evalWith options "()" = null @>

[<Theory;EvalData>]
let ``literal default value type`` options =
    test <@ evalWith options "default[int32]" = 0 @>

[<Theory;EvalData>]
let ``literal default ref type`` options =
    test <@ evalWith options "default[string]" = null @>

[<Theory;EvalData>]
let ``literal default void is invalid`` options =
    raisesWith 
        <@ evalWith options "default[system.void]" @>
        (expectedErrors [|14|])