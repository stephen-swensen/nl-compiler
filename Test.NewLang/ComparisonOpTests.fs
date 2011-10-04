module ComparisonOpTests

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

[<Fact(Skip="todo")>]
let ``boxed value types of same value are equal`` () =
    test <@ C.eval "3[object] == 3[object]" = true @>

[<Fact(Skip="todo")>]
let ``can't compare boxed object to value type`` () =
    test <@ C.eval "3[object] == 3" = true @>



//still got a bunch more cases to go