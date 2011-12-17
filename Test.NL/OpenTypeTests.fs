module Tests.OpenTypeTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic

module C = Compilation

[<Fact>]
let ``resolve method of open static class`` () =
    test <@ C.eval "open system.math in sin(3.0)" = sin(3.0) @>

[<Fact>]
let ``resolve method of open generic static class`` () =
    test <@ C.eval "open System.Collections.Generic.Comparer[int32] in get_Default()" = System.Collections.Generic.Comparer<int>.Default @>

[<Fact>]
let ``resolve field of open static class`` () =
    test <@ C.eval (openPrefix + "open NonGenericClass1 in static_field_int") = 0 @>

[<Fact>]
let ``resolve property of open static class`` () =
    test <@ C.eval (openPrefix + "open NonGenericClass1 in static_property_int") = 0 @>

    