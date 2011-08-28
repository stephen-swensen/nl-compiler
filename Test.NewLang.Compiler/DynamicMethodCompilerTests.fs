module DynamicMethodCompilerTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compiler

[<Fact>]
let ``literal System.Int32 exp`` () =
    test <@ C.eval "3" = box 3 @>

[<Fact>]
let ``literal System.Double exp`` () =
    test <@ C.eval "3.0" = box 3.0 @>

[<Fact>]
let ``literal System.String exp`` () =
    test <@ C.eval "\"hello world\"" = box "hello world" @>

[<Fact>]
let ``Int32 addition`` () =
    test <@ C.eval "3 + 3" = box 6 @>

[<Fact>]
let ``Double addition`` () =
    test <@ C.eval "3.0 + 3.0" = box 6.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double`` () =
    test <@ C.eval "3.0 + 3" = box 6.0 @>

[<Fact>]
let ``String concat`` () =
    test <@ C.eval "\"hello \" ++ \"world\"" = box "hello world" @>

[<Fact>]
let ``var binding to literal value`` () =
    test <@ C.eval "x = 3 in x" = box 3 @>

[<Fact>]
let ``var binding to complex expression`` () =
    test <@ C.eval "x = 3 + 3 in x" = box 6 @>

[<Fact>]
let ``var binding used in complex body expression`` () =
    test <@ C.eval "x = 3 in x + x + 3" = box 9 @>

[<Fact>]
let ``nested var binding`` () =
    test <@ C.eval "x = 3 in y = 5 in x + y" = box 8 @>

[<Fact>]
let ``var shadowing`` () =
    test <@ C.eval "x = 3 in y = 5 in x = 5 in x + y" = box 10 @>

