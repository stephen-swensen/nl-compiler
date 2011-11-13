module Tests.NumericOpTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compilation

[<Fact>]
let ``Int32 addition`` () =
    test <@ C.eval "3 + 3" = 6 @>

[<Fact>]
let ``Double addition`` () =
    test <@ C.eval "3.0 + 3.0" = 6.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in addition`` () =
    test <@ C.eval "3.0 + 3" = 6.0 @>

[<Fact>]
let ``Int32 subtraction`` () =
    test <@ C.eval "3 - 3" = 0 @>

[<Fact>]
let ``Double subtraction`` () =
    test <@ C.eval "3.0 - 3.0" = 0.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in sub`` () =
    test <@ C.eval "3.0 - 3" = 0.0 @>

[<Fact>]
let ``Int32 mult`` () =
    test <@ C.eval "3 * 3" = 9 @>

[<Fact>]
let ``Double mult`` () =
    test <@ C.eval "3.0 * 3.0" = 9.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in mult`` () =
    test <@ C.eval "3.0 * 3" = 9.0 @>

[<Fact>]
let ``Int32 div`` () =
    test <@ C.eval "3 / 3" = 1 @>

[<Fact>]
let ``Double div`` () =
    test <@ C.eval "3.0 / 3.0" = 1.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in div`` () =
    test <@ C.eval "3.0 / 3" = 1.0 @>

[<Fact>]
let ``Pow on floats only`` () =
    test <@ C.eval "3.0 ** 3.0" = 27.0 @>

//[<Fact>]
//let ``Factorial on ints only`` () =
//    test <@ C.eval "4!" = 24 @>

[<Fact>]
let ``unary minus int`` () =
    test <@ C.eval "-3" = -3 @>

[<Fact>]
let ``unary minus float`` () =
    test <@ C.eval "-3.0" = -3.0 @>

[<Fact>]
let ``* stronger than +`` () =
    test <@ C.eval "2 + 3 * 4" = 14 @>

[<Fact>]
let ``parenthesis to overcome precedence`` () =
    test <@ C.eval "(2 + 3) * 4" = 20 @>

[<Fact>]
let ``+ method overload`` () =
    test <@ C.eval "biginteger(1) + biginteger(2)" = 3I @>

[<Fact>]
let ``- method overload`` () =
    test <@ C.eval "biginteger(1) - biginteger(2)" = -1I @>

[<Fact>]
let ``* method overload`` () =
    test <@ C.eval "biginteger(1) * biginteger(2)" = 2I @>

[<Fact>]
let ``/ method overload`` () =
    test <@ C.eval "biginteger(1) / biginteger(2)" = 0I @>

[<Fact>]
let ``String concat`` () =
    test <@ C.eval "\"hello \" + \"world\"" = "hello world" @>

[<Fact>]
let ``uminus primitive int64`` () =
    test <@ C.eval "-int64.parse(\"1\") == int64.parse(\"-1\")" @>

[<Fact>]
let ``uminus primitive int32`` () =
    test <@ C.eval "-int32.parse(\"1\") == int32.parse(\"-1\")" @>

[<Fact>]
let ``uminus primitive int16`` () =
    test <@ C.eval "-int16.parse(\"1\") == int16.parse(\"-1\")" @>

[<Fact>]
let ``uminus primitive double`` () =
    test <@ C.eval "-double.parse(\"1\") == double.parse(\"-1\")" @>

[<Fact>]
let ``uminus primitive single`` () =
    test <@ C.eval "-single.parse(\"1\") == single.parse(\"-1\")" @>

[<Fact>]
let ``uminus non primitive biginteger`` () =
    test <@ C.eval "-biginteger.parse(\"1\") == biginteger.parse(\"-1\")" @>

[<Fact>]
let ``uminus error`` () =
    raisesWith
        <@ C.eval "-'c'" @>
        (expectedErrors [|25|])