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

[<Theory;EvalData>]
let ``else if syntax`` options =
    test <@ evalWith options "if false { false } else if true { true }" = true @>

[<Theory;EvalData>]
let ``else if syntax deeply nested no else`` options =
    test <@ evalWith options "if false { false } else if false { false } else if false { false }" = false @>

[<Theory;EvalData>]
let ``else if syntax deeply nested with else`` options =
    test <@ evalWith options "if false { false } else if false { false } else if false { false } else { true }" = true @>

[<Theory;EvalData>]
let ``if branch throw else branch non void`` options =
    test <@ evalWith options "if false { throw(exception()) } else { 0 }" = 0 @>

[<Theory;EvalData>]
let ``if branch 0 else branch throw`` options =
    test <@ evalWith options "if true { 0 } else { throw(exception()) }" = 0 @>

[<Theory;EvalData>]
let ``if branch 0 else branch void type mismatch error`` options =
    raisesWith <@ evalWith options "if true { 1 } else { () }" @>
        (expectedErrors [|23|])

[<Theory;EvalData>]
let ``if branch void else branch non void type mismatch error`` options =
    raisesWith <@ evalWith options "if true { () } else { 1 }" @>
        (expectedErrors [|23|])

[<Theory;EvalData>]
let ``if branch 0 else branch escape default prevails`` options =
    test <@ evalWith options "if true { 0 } else { throw(exception()) }" = 0 @>