module Tests.SequentialTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open Evaluation

[<Theory;EvalData>]
let ``sequential expression`` options =
    test <@ evalWith options "3;4" = 4 @>

[<Theory;EvalData>]
let ``multiple sequential expressions`` options =
    test <@ evalWith options "3;4;5" = 5 @>

[<Theory;EvalData>]
let ``sequential expressions have weak right associativity`` options =
    test <@ evalWith options "2 + 3 ; 3 + 5" = 8 @>

[<Theory;EvalData>]
let ``sequential expression with lhs void (result does not need to be popped from the stack)`` options =
    test <@ evalWith options "system.console.writeline(1); 4" = 4 @>

[<Theory;EvalData>]
let ``sequential expression with lhs nop`` options =
    test <@ evalWith options "(); 4" = 4 @>