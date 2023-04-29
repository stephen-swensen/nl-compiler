module Tests.VarTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open Evaluation

[<Theory;EvalData>]
let ``var binding to literal value`` options =
    test <@ evalWith options "x = 3 in x" = 3 @>

[<Theory;EvalData>]
let ``var binding to complex expression`` options =
    test <@ evalWith options "x = 3 + 3 in x" = 6 @>

[<Theory;EvalData>]
let ``var binding used in complex body expression`` options =
    test <@ evalWith options "x = 3 in x + x + 3" = 9 @>

[<Theory;EvalData>]
let ``nested var binding`` options =
    test <@ evalWith options "x = 3 in y = 5 in x + y" = 8 @>

[<Theory;EvalData>]
let ``var shadowing`` options =
    test <@ evalWith options "x = 3 in y = 5 in x = 5 in x + y" = 10 @>

[<Theory;EvalData>]
let ``var ids are case insensitive`` options =
    test <@ evalWith options "x.x.x = 3 in X.x.X" = 3 @>

[<Theory;EvalData>]
let ``Void not valid in let binding`` options =
    raisesWith 
        <@ evalWith options "x = console.writeline(\"asdf\") in x" @>
        (expectedErrors [|16|])

[<Theory;EvalData>]
let ``Var set`` options =
    test <@ evalWith options "x = 3 in (x <- 2) ; x" = 2 @>

[<Theory;EvalData>]
let ``Var set stronger precedence than semicolon`` options =
    test <@ evalWith options "x = 3 in x <- 2 ; x" = 2 @>

[<Theory;EvalData>]
let ``var not found`` options =
    raisesWith 
        <@ evalWith options "x; ()" @>
        (expectedErrors [|5|])

[<Theory;EvalData>]
let ``var set type mismatch error`` options =
    raisesWith 
        <@ evalWith options "x = 3 in x <- 'c'" @>
        (expectedErrors [|4|])

[<Theory;EvalData>]
let ``var set var not found error`` options =
    raisesWith 
        <@ evalWith options "x <- 'c'" @>
        (expectedErrors [|5|])