module Tests.NVTScopingTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``open type non-generic static method is shadowed by open namespace non-generic constructor`` () =
    test <@ (C.eval<obj> <| openPrefix + "open NonGenericClass1 in open System.Diagnostics in Stopwatch()").GetType() = typeof<System.Diagnostics.Stopwatch> @>

[<Fact>]
let ``open namespace non-generic constructor is shadowed by open type non-generic method`` () =
    test <@ (C.eval<obj> <| openPrefix + "open System.Diagnostics in open NonGenericClass1 in Stopwatch()").GetType() = typeof<System.Int32> @>

[<Fact>]
let ``two open types with one having a field and another having a method of the same name do not shadow eachother combo 1`` () =
    test <@ C.eval (openPrefix + "open NonGenericClass1 in open NonGenericClass2 in same_name_different_kind()") = 0 @>

[<Fact>]
let ``two open types with one having a field and another having a method of the same name do not shadow eachother combo 2`` () =
    test <@ C.eval (openPrefix + "open NonGenericClass1 in open NonGenericClass2 in same_name_different_kind") = 0 @>

[<Fact>]
let ``open type field shadowing combo 1`` () =
    test <@ C.eval (openPrefix + "open NonGenericClass2 in open NonGenericClass1 in same_name_different_value") = 1 @>

[<Fact>]
let ``open type field shadowing combo 2`` () =
    test <@ C.eval (openPrefix + "open NonGenericClass1 in open NonGenericClass2 in same_name_different_value") = 2 @>

