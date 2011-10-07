module Tests.VarTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compilation

[<Fact>]
let ``var binding to literal value`` () =
    test <@ C.eval "x = 3 in x" = 3 @>

[<Fact>]
let ``var binding to complex expression`` () =
    test <@ C.eval "x = 3 + 3 in x" = 6 @>

[<Fact>]
let ``var binding used in complex body expression`` () =
    test <@ C.eval "x = 3 in x + x + 3" = 9 @>

[<Fact>]
let ``nested var binding`` () =
    test <@ C.eval "x = 3 in y = 5 in x + y" = 8 @>

[<Fact>]
let ``var shadowing`` () =
    test <@ C.eval "x = 3 in y = 5 in x = 5 in x + y" = 10 @>

[<Fact>]
let ``var ids are case insensitive`` () =
    test <@ C.eval "x.x.x = 3 in X.x.X" = 3 @>

[<Fact>]
let ``Void not valid in let binding`` () =
    raisesWith 
        <@ C.eval "x = console.writeline(\"asdf\") in x" @>
        (expectedErrors [|16|])

[<Fact>]
let ``Var set`` () =
    test <@ C.eval "x = 3 in (x <- 2) ; x" = 2 @>

[<Fact>]
let ``Var set stronger precedence than semicolon`` () =
    test <@ C.eval "x = 3 in x <- 2 ; x" = 2 @>

//[<Fact>]
//let ``Var set weaker precedence than fact`` () =
//    test <@ C.eval "x = 3 in x <- 2! ; x" = 2 @>