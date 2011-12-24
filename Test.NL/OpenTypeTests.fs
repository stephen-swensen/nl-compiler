module Tests.OpenTypeTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

open Evaluation

[<Theory;EvalData>]
let ``resolve method of open static class`` options =
    test <@ evalWith options "open system.math in sin(3.0)" = sin(3.0) @>

[<Theory;EvalData>]
let ``resolve method of open generic static class`` options =
    test <@ evalWith options "open System.Collections.Generic.Comparer[int32] in get_Default()" = System.Collections.Generic.Comparer<int>.Default @>

[<Theory;EvalData>]
let ``resolve field of open static class`` options =
    test <@ evalWith options (openPrefix + "open NonGenericClass1 in static_field_int") = 0 @>

[<Theory;EvalData>]
let ``resolve property of open static class`` options =
    test <@ evalWith options (openPrefix + "open NonGenericClass1 in static_property_int") = 0 @>

[<Theory;EvalData>]
let ``set field of open static class`` options =
    test <@ evalWith options (openPrefix + 
                "open NonGenericClass1 in 
                 static_field_int <- 3;
                 temp = static_field_int in
                 static_field_int <- 0;
                 temp
                ") = 3 @>

[<Theory;EvalData>]
let ``set property of open static class`` options =
    test <@ evalWith options (openPrefix + 
                "open NonGenericClass1 in 
                 static_property_int <- 3;
                 temp = static_property_int in
                 static_property_int <- 0;
                 temp
                ") = 3 @>

    