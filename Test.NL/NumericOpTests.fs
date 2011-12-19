module Tests.NumericOpTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
module C = Compilation

[<Theory;EvalData>]
let ``Int32 addition`` options =
    test <@ C.evalWith options "3 + 3" = 6 @>

[<Theory;EvalData>]
let ``Double addition`` options =
    test <@ C.evalWith options "3.0 + 3.0" = 6.0 @>

[<Theory;EvalData>]
let ``Lossless coersion of Int32 to Double in addition`` options =
    test <@ C.evalWith options "3.0 + 3" = 6.0 @>

[<Theory;EvalData>]
let ``Int32 subtraction`` options =
    test <@ C.evalWith options "3 - 3" = 0 @>

[<Theory;EvalData>]
let ``Double subtraction`` options =
    test <@ C.evalWith options "3.0 - 3.0" = 0.0 @>

[<Theory;EvalData>]
let ``Lossless coersion of Int32 to Double in sub`` options =
    test <@ C.evalWith options "3.0 - 3" = 0.0 @>

[<Theory;EvalData>]
let ``Int32 mult`` options =
    test <@ C.evalWith options "3 * 3" = 9 @>

[<Theory;EvalData>]
let ``Double mult`` options =
    test <@ C.evalWith options "3.0 * 3.0" = 9.0 @>

[<Theory;EvalData>]
let ``Lossless coersion of Int32 to Double in mult`` options =
    test <@ C.evalWith options "3.0 * 3" = 9.0 @>

[<Theory;EvalData>]
let ``Int32 div`` options =
    test <@ C.evalWith options "3 / 3" = 1 @>

[<Theory;EvalData>]
let ``Double div`` options =
    test <@ C.evalWith options "3.0 / 3.0" = 1.0 @>

[<Theory;EvalData>]
let ``Lossless coersion of Int32 to Double in div`` options =
    test <@ C.evalWith options "3.0 / 3" = 1.0 @>

[<Theory;EvalData>]
let ``Pow on floats only`` options =
    test <@ C.evalWith options "3.0 ** 3.0" = 27.0 @>

//[<Theory;EvalData>]
//let ``Factorial on ints only`` options =
//    test <@ C.evalWith options "4!" = 24 @>

[<Theory;EvalData>]
let ``unary minus int`` options =
    test <@ C.evalWith options "-3" = -3 @>

[<Theory;EvalData>]
let ``unary minus float`` options =
    test <@ C.evalWith options "-3.0" = -3.0 @>

[<Theory;EvalData>]
let ``* stronger than +`` options =
    test <@ C.evalWith options "2 + 3 * 4" = 14 @>

[<Theory;EvalData>]
let ``parenthesis to overcome precedence`` options =
    test <@ C.evalWith options "(2 + 3) * 4" = 20 @>

[<Theory;EvalData>]
let ``+ method overload`` options =
    test <@ C.evalWith options "biginteger(1) + biginteger(2)" = 3I @>

[<Theory;EvalData>]
let ``- method overload`` options =
    test <@ C.evalWith options "biginteger(1) - biginteger(2)" = -1I @>

[<Theory;EvalData>]
let ``* method overload`` options =
    test <@ C.evalWith options "biginteger(1) * biginteger(2)" = 2I @>

[<Theory;EvalData>]
let ``/ method overload`` options =
    test <@ C.evalWith options "biginteger(1) / biginteger(2)" = 0I @>

[<Theory;EvalData>]
let ``String concat`` options =
    test <@ C.evalWith options "\"hello \" + \"world\"" = "hello world" @>

[<Theory;EvalData>]
let ``uminus primitive int64`` options =
    test <@ C.evalWith options "-int64.parse(\"1\") == int64.parse(\"-1\")" @>

[<Theory;EvalData>]
let ``uminus primitive int32`` options =
    test <@ C.evalWith options "-int32.parse(\"1\") == int32.parse(\"-1\")" @>

[<Theory;EvalData>]
let ``uminus primitive int16`` options =
    test <@ C.evalWith options "-int16.parse(\"1\") == int16.parse(\"-1\")" @>

[<Theory;EvalData>]
let ``uminus primitive double`` options =
    test <@ C.evalWith options "-double.parse(\"1\") == double.parse(\"-1\")" @>

[<Theory;EvalData>]
let ``uminus primitive single`` options =
    test <@ C.evalWith options "-single.parse(\"1\") == single.parse(\"-1\")" @>

[<Theory;EvalData>]
let ``uminus non primitive biginteger`` options =
    test <@ C.evalWith options "-biginteger.parse(\"1\") == biginteger.parse(\"-1\")" @>

[<Theory;EvalData>]
let ``uminus error`` options =
    raisesWith
        <@ C.evalWith options "-'c'" @>
        (expectedErrors [|25|])

[<Theory;EvalData>]
let ``could not resolve binary operator overload`` options =
    raisesWith
        <@ C.evalWith options "biginteger.parse(\"1\") + 1" @>
        (expectedErrors [|3|])