module Tests.NVTScopingTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Theory;EvalData>]
let ``open type non-generic static method is shadowed by open namespace non-generic constructor`` options =
    test <@ (C.evalWith<obj> options <| openPrefix + "open NonGenericClass1 in open System.Diagnostics in Stopwatch()").GetType() = typeof<System.Diagnostics.Stopwatch> @>

[<Theory;EvalData>]
let ``open namespace non-generic constructor is shadowed by open type non-generic method`` options =
    test <@ (C.evalWith<obj> options <| openPrefix + "open System.Diagnostics in open NonGenericClass1 in Stopwatch()").GetType() = typeof<System.Int32> @>

[<Theory;EvalData>]
let ``two open types with one having a field and another having a method of the same name do not shadow eachother combo 1`` options =
    test <@ C.evalWith options (openPrefix + "open NonGenericClass1 in open NonGenericClass2 in same_name_different_kind()") = 0 @>

[<Theory;EvalData>]
let ``two open types with one having a field and another having a method of the same name do not shadow eachother combo 2`` options =
    test <@ C.evalWith options (openPrefix + "open NonGenericClass1 in open NonGenericClass2 in same_name_different_kind") = 0 @>

[<Theory;EvalData>]
let ``open type field shadowing combo 1`` options =
    test <@ C.evalWith options (openPrefix + "open NonGenericClass2 in open NonGenericClass1 in same_name_different_value") = 1 @>

[<Theory;EvalData>]
let ``open type field shadowing combo 2`` options =
    test <@ C.evalWith options (openPrefix + "open NonGenericClass1 in open NonGenericClass2 in same_name_different_value") = 2 @>

