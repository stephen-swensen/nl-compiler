module Tests.ComparisonOpTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open Evaluation

[<Theory;EvalData>]
let ``primitive == true`` options =
    test <@ evalWith options "1 == 1" = true @>

[<Theory;EvalData>]
let ``primitive == false`` options =
    test <@ evalWith options "1 == 2" = false @>

[<Theory;EvalData>]
let ``primitive > true`` options =
    test <@ evalWith options "1 > 0" = true @>

[<Theory;EvalData>]
let ``primitive > false`` options =
    test <@ evalWith options "1 > 2" = false @>

[<Theory;EvalData>]
let ``primitive >= false`` options =
    test <@ evalWith options "1 >= 2" = false @>

[<Theory;EvalData>]
let ``primitive >= true when ==`` options =
    test <@ evalWith options "1 >= 1" = true @>

[<Theory;EvalData>]
let ``primitive >= true when >`` options =
    test <@ evalWith options "1 >= 0" = true @>

[<Theory;EvalData>]
let ``primitive < true`` options =
    test <@ evalWith options "0 < 1" = true @>

[<Theory;EvalData>]
let ``primitive < false`` options =
    test <@ evalWith options "1 < 1" = false @>

[<Theory;EvalData>]
let ``primitive <= false`` options =
    test <@ evalWith options "3 <= 2" = false @>

[<Theory;EvalData>]
let ``primitive <= true when ==`` options =
    test <@ evalWith options "1 <= 1" = true @>

[<Theory;EvalData>]
let ``primitive <= true when <`` options =
    test <@ evalWith options "-1 <= 0" = true @>

[<Theory;EvalData>]
let ``static operator == true`` options =
    test <@ evalWith options "biginteger(1) == biginteger(1)" = true @>

[<Theory;EvalData>]
let ``static operator == false`` options =
    test <@ evalWith options "biginteger(1) == biginteger(2)" = false @>

[<Theory;EvalData>]
let ``static operator > true`` options =
    test <@ evalWith options "biginteger(1) > biginteger(0)" = true @>

[<Theory;EvalData>]
let ``static operator > false`` options =
    test <@ evalWith options "biginteger(1) > biginteger(2)" = false @>

[<Theory;EvalData>]
let ``static operator >= false`` options =
    test <@ evalWith options "biginteger(1) >= biginteger(2)" = false @>

[<Theory;EvalData>]
let ``static operator >= true when ==`` options =
    test <@ evalWith options "biginteger(1) >= biginteger(1)" = true @>

[<Theory;EvalData>]
let ``static operator >= true when >`` options =
    test <@ evalWith options "biginteger(1) >= biginteger(0)" = true @>

[<Theory;EvalData>]
let ``static operator < true`` options =
    test <@ evalWith options "biginteger(0) < biginteger(1)" = true @>

[<Theory;EvalData>]
let ``static operator < false`` options =
    test <@ evalWith options "biginteger(1) < biginteger(1)" = false @>

[<Theory;EvalData>]
let ``static operator <= false`` options =
    test <@ evalWith options "biginteger(3) <= biginteger(2)" = false @>

[<Theory;EvalData>]
let ``static operator <= true when ==`` options =
    test <@ evalWith options "biginteger(1) <= biginteger(1)" = true @>

[<Theory;EvalData>]
let ``static operator <= true when <`` options =
    test <@ evalWith options "biginteger(-1) <= biginteger(0)" = true @>

//note: in F# box 3 = box 3 is true, whereas in C# (object) 3 == (object) 3 is false
//we'll keep the C# semantics for now
//[<Fact(Skip="todo")>]
//let ``boxed value types of same value are equal`` options =
//    test <@ evalWith options "3[object] == 3[object]" = true @>

[<Theory;EvalData>]
let ``can't compare boxed object to value type`` options =
    raisesWith 
        <@ evalWith options "3[object] == 3" = true @>
        (expectedErrors [|3|])

[<Theory;EvalData>]
let ``can't compare value type to boxed object`` options =
    raisesWith 
        <@ evalWith options "3 == 3[object]" = true @>
        (expectedErrors [|3|])

[<Theory(Skip="todo");EvalData>]
let ``issue 51: > comparison of two unsigned int32s`` options =
    test <@ evalWith options "uint32.parse(\"4294967295\") > uint32.parse(\"0\")" = true @>

[<Theory;EvalData>]
let ``gt cannot be applied to chars`` options =
    raisesWith 
        <@ evalWith options "'a' > 'b'" @>
        (expectedErrors [|3|])

[<Theory;EvalData>]
let ``lt cannot be applied to chars`` options =
    raisesWith 
        <@ evalWith options "'a' < 'b'" @>
        (expectedErrors [|3|])

[<Theory;EvalData>]
let ``eq chars true`` options =
    test <@ evalWith options "'a' == 'a'" = true @>

[<Theory;EvalData>]
let ``eq chars false`` options =
    test <@ evalWith options "'a' == 'b'" = false @>

//still got a bunch more cases to go