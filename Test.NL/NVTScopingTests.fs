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