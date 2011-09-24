module IfThenElseTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compilation

[<Fact>]
let ``ifthenelse condition must be boolean`` () =
    raises<SemanticErrorException> <@ C.eval "if 0 then true else false" @>

[<Fact>]
let ``ifthenelse type must be consistent`` () =
    raises<SemanticErrorException> <@ C.eval "if true then true else 0" @>

[<Fact>]
let ``simple ifthenelse exp condition is true`` () =
    test <@ C.eval "if true then true else false" = true @>

[<Fact>]
let ``simple ifthenelse exp condition is false`` () =
    test <@ C.eval "if false then true else false" = false @>

[<Fact>]
let ``ifthen void`` () =
    test <@ C.eval "if true then console.writeline(1)" = () @>

[<Fact>]
let ``ifthenelse with explicit else void`` () =
    test <@ C.eval "if false then console.writeline(1) else ()" = () @>

[<Fact>]
let ``ifthen default valuetype`` () =
    test <@ C.eval "if false then true" = false @>

[<Fact>]
let ``ifthen default reftype`` () =
    test <@ C.eval "if false then \"asdf\"" = null @>

