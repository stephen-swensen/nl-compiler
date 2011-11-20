module Tests.IfThenElseTests

open Xunit
open Swensen.Unquote
open Swensen.NL
module C = Compilation

[<Fact>]
let ``ifthenelse condition must be boolean`` () =
    raisesWith 
        <@ C.eval "if 0 { true } else { false }" @>
        (expectedErrors [|6|])

[<Fact>]
let ``ifthenelse type must be consistent`` () =
    raisesWith 
        <@ C.eval "if true { true } else { 0 }" @>
        (expectedErrors [|23|])

[<Fact>]
let ``simple ifthenelse exp condition is true`` () =
    test <@ C.eval "if true { true } else { false }" = true @>

[<Fact>]
let ``simple ifthenelse exp condition is false`` () =
    test <@ C.eval "if false { true } else { false }" = false @>

[<Fact>]
let ``ifthen void`` () =
    test <@ C.eval "if true { console.writeline(1) }" = () @>

[<Fact>]
let ``ifthenelse with explicit else void`` () =
    test <@ C.eval "if false { console.writeline(1) } else { () }" = () @>

[<Fact>]
let ``ifthen default valuetype`` () =
    test <@ C.eval "if false { true }" = false @>

[<Fact>]
let ``ifthen default reftype`` () =
    test <@ C.eval "if false { \"asdf\" }" = null @>

