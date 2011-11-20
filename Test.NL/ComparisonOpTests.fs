module Tests.ComparisonOpTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compilation

[<Fact>]
let ``primitive == true`` () =
    test <@ C.eval "1 == 1" = true @>

[<Fact>]
let ``primitive == false`` () =
    test <@ C.eval "1 == 2" = false @>

[<Fact>]
let ``primitive > true`` () =
    test <@ C.eval "1 > 0" = true @>

[<Fact>]
let ``primitive > false`` () =
    test <@ C.eval "1 > 2" = false @>

[<Fact>]
let ``primitive >= false`` () =
    test <@ C.eval "1 >= 2" = false @>

[<Fact>]
let ``primitive >= true when ==`` () =
    test <@ C.eval "1 >= 1" = true @>

[<Fact>]
let ``primitive >= true when >`` () =
    test <@ C.eval "1 >= 0" = true @>

[<Fact>]
let ``primitive < true`` () =
    test <@ C.eval "0 < 1" = true @>

[<Fact>]
let ``primitive < false`` () =
    test <@ C.eval "1 < 1" = false @>

[<Fact>]
let ``primitive <= false`` () =
    test <@ C.eval "3 <= 2" = false @>

[<Fact>]
let ``primitive <= true when ==`` () =
    test <@ C.eval "1 <= 1" = true @>

[<Fact>]
let ``primitive <= true when <`` () =
    test <@ C.eval "-1 <= 0" = true @>

[<Fact>]
let ``static operator == true`` () =
    test <@ C.eval "biginteger(1) == biginteger(1)" = true @>

[<Fact>]
let ``static operator == false`` () =
    test <@ C.eval "biginteger(1) == biginteger(2)" = false @>

[<Fact>]
let ``static operator > true`` () =
    test <@ C.eval "biginteger(1) > biginteger(0)" = true @>

[<Fact>]
let ``static operator > false`` () =
    test <@ C.eval "biginteger(1) > biginteger(2)" = false @>

[<Fact>]
let ``static operator >= false`` () =
    test <@ C.eval "biginteger(1) >= biginteger(2)" = false @>

[<Fact>]
let ``static operator >= true when ==`` () =
    test <@ C.eval "biginteger(1) >= biginteger(1)" = true @>

[<Fact>]
let ``static operator >= true when >`` () =
    test <@ C.eval "biginteger(1) >= biginteger(0)" = true @>

[<Fact>]
let ``static operator < true`` () =
    test <@ C.eval "biginteger(0) < biginteger(1)" = true @>

[<Fact>]
let ``static operator < false`` () =
    test <@ C.eval "biginteger(1) < biginteger(1)" = false @>

[<Fact>]
let ``static operator <= false`` () =
    test <@ C.eval "biginteger(3) <= biginteger(2)" = false @>

[<Fact>]
let ``static operator <= true when ==`` () =
    test <@ C.eval "biginteger(1) <= biginteger(1)" = true @>

[<Fact>]
let ``static operator <= true when <`` () =
    test <@ C.eval "biginteger(-1) <= biginteger(0)" = true @>

//note: in F# box 3 = box 3 is true, whereas in C# (object) 3 == (object) 3 is false
//we'll keep the C# semantics for now
//[<Fact(Skip="todo")>]
//let ``boxed value types of same value are equal`` () =
//    test <@ C.eval "3[object] == 3[object]" = true @>

[<Fact>]
let ``can't compare boxed object to value type`` () =
    raisesWith 
        <@ C.eval "3[object] == 3" = true @>
        (expectedErrors [|3|])

[<Fact>]
let ``can't compare value type to boxed object`` () =
    raisesWith 
        <@ C.eval "3 == 3[object]" = true @>
        (expectedErrors [|3|])

//still got a bunch more cases to go