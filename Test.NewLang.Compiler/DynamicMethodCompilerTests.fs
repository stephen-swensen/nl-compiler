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
let ``Lossless coersion of Int32 to Double in addition`` () =
    test <@ C.eval "3.0 + 3" = box 6.0 @>

[<Fact>]
let ``Int32 subtraction`` () =
    test <@ C.eval "3 - 3" = box 0 @>

[<Fact>]
let ``Double subtraction`` () =
    test <@ C.eval "3.0 - 3.0" = box 0.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in sub`` () =
    test <@ C.eval "3.0 - 3" = box 0.0 @>

[<Fact>]
let ``Int32 mult`` () =
    test <@ C.eval "3 * 3" = box 9 @>

[<Fact>]
let ``Double mult`` () =
    test <@ C.eval "3.0 * 3.0" = box 9.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in mult`` () =
    test <@ C.eval "3.0 * 3" = box 9.0 @>

[<Fact>]
let ``Int32 div`` () =
    test <@ C.eval "3 / 3" = box 1 @>

[<Fact>]
let ``Double div`` () =
    test <@ C.eval "3.0 / 3.0" = box 1.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in div`` () =
    test <@ C.eval "3.0 / 3" = box 1.0 @>

[<Fact>]
let ``Pow on floats only`` () =
    test <@ C.eval "3.0 ^ 3.0" = box 27.0 @>

[<Fact>]
let ``Factorial on ints only`` () =
    test <@ C.eval "4!" = box 24 @>

[<Fact>]
let ``unary minus int`` () =
    test <@ C.eval "-3" = box -3 @>

[<Fact>]
let ``unary minus float`` () =
    test <@ C.eval "-3.0" = box -3.0 @>

[<Fact>]
let ``* stronger than +`` () =
    test <@ C.eval "2 + 3 * 4" = box 14 @>

[<Fact>]
let ``parenthesis to overcome precedence`` () =
    test <@ C.eval "(2 + 3) * 4" = box 20 @>

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

[<Fact>]
let ``var ids are case insensitive`` () =
    test <@ C.eval "x.x.x = 3 in X.x.X" = box 3 @>

[<Fact>]
let ``instance call on value type`` () =
    test <@ C.eval "3.ToString()" = box "3" @>

[<Fact>]
let ``instance calls are case insensitive`` () =
    test <@ C.eval "3.tostrINg()" = box "3" @>

[<Fact>]
let ``instance call on obj type`` () =
    test <@ C.eval "\"3\".ToString()" = box "3" @>

[<Fact>]
let ``static call`` () =
    test <@ C.eval "System.String.Concat(\"hello \", \"world\")" = box "hello world" @>

[<Fact>]
let ``static call is case insensitive`` () =
    test <@ C.eval "System.STRING.Concat(\"hello \", \"world\")" = box "hello world" @>

[<Fact>]
let ``sequential expression`` () =
    test <@ C.eval "3;4" = box 4 @>

[<Fact>]
let ``multiple sequential expressions`` () =
    test <@ C.eval "3;4;5" = box 5 @>

[<Fact>]
let ``sequential expressions have weak right associativity`` () =
    test <@ C.eval "2 + 3 ; 3 + 5" = box 8 @>

[<Fact>]
let ``sequential expression with rhs void (result does not need to be popped from the stack)`` () =
    test <@ C.eval "system.console.writeline(\"3\"); 4" = box 4 @>
