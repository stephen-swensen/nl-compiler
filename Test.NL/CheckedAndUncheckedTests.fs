module Tests.CheckedAndUncheckedTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

open Evaluation

[<Theory(Skip="not implemented yet");EvalData>]
let ``checked overflow int32 to int8 coersion`` options =
    raises<System.OverflowException> <@ evalWith options "checked { 99999[sbyte] }" @>

[<Theory;EvalData>]
let ``default unchecked overflow int32 to int8 coersion`` options =
    test <@ evalWith options "99999[sbyte]" = -97y @>

[<Theory;EvalData>]
let ``default unchecked overflow equals explicit checked overflow int32 to int8 coersion`` options =
    test <@ evalWith options "99999[sbyte]" = evalWith options "unchecked { 99999[sbyte] }" @>

[<Theory;EvalData>]
let ``nested scope`` options =
    test <@ evalWith options "checked { unchecked { 99999[sbyte] } }" = -97y @>

[<Theory;EvalData>]
let ``checked and unchecked as expressions`` options =
    test <@ evalWith options "checked { unchecked { 1 } + checked { 1 } }" = 2 @>



///need to have optimization tests showing no checked ops