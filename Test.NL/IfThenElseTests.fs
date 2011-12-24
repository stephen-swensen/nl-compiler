module Tests.IfThenElseTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open Evaluation

[<Theory;EvalData>]
let ``ifthenelse condition must be boolean`` options =
    raisesWith 
        <@ evalWith options "if 0 { true } else { false }" @>
        (expectedErrors [|6|])

[<Theory;EvalData>]
let ``ifthenelse type must be consistent`` options =
    raisesWith 
        <@ evalWith options "if true { true } else { 0 }" @>
        (expectedErrors [|23|])

[<Theory;EvalData>]
let ``simple ifthenelse exp condition is true`` options =
    test <@ evalWith options "if true { true } else { false }" = true @>

[<Theory;EvalData>]
let ``simple ifthenelse exp condition is false`` options =
    test <@ evalWith options "if false { true } else { false }" = false @>

[<Theory;EvalData>]
let ``ifthen void`` options =
    test <@ evalWith options "if true { console.writeline(1) }" = () @>

[<Theory;EvalData>]
let ``ifthenelse with explicit else void`` options =
    test <@ evalWith options "if false { console.writeline(1) } else { () }" = () @>

[<Theory;EvalData>]
let ``ifthen default valuetype`` options =
    test <@ evalWith options "if false { true }" = false @>

[<Theory;EvalData>]
let ``ifthen default reftype`` options =
    test <@ evalWith options "if false { \"asdf\" }" = null @>

