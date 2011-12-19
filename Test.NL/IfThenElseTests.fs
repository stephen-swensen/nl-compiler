module Tests.IfThenElseTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
module C = Compilation

[<Theory;EvalData>]
let ``ifthenelse condition must be boolean`` options =
    raisesWith 
        <@ C.evalWith options "if 0 { true } else { false }" @>
        (expectedErrors [|6|])

[<Theory;EvalData>]
let ``ifthenelse type must be consistent`` options =
    raisesWith 
        <@ C.evalWith options "if true { true } else { 0 }" @>
        (expectedErrors [|23|])

[<Theory;EvalData>]
let ``simple ifthenelse exp condition is true`` options =
    test <@ C.evalWith options "if true { true } else { false }" = true @>

[<Theory;EvalData>]
let ``simple ifthenelse exp condition is false`` options =
    test <@ C.evalWith options "if false { true } else { false }" = false @>

[<Theory;EvalData>]
let ``ifthen void`` options =
    test <@ C.evalWith options "if true { console.writeline(1) }" = () @>

[<Theory;EvalData>]
let ``ifthenelse with explicit else void`` options =
    test <@ C.evalWith options "if false { console.writeline(1) } else { () }" = () @>

[<Theory;EvalData>]
let ``ifthen default valuetype`` options =
    test <@ C.evalWith options "if false { true }" = false @>

[<Theory;EvalData>]
let ``ifthen default reftype`` options =
    test <@ C.evalWith options "if false { \"asdf\" }" = null @>

