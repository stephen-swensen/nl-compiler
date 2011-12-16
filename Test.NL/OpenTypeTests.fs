module Tests.OpenTypeTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic

module C = Compilation

[<Fact>]
let ``open static class`` () =
    test <@ C.eval "open system.math in sin(3.0)" = sin(3.0) @>

[<Fact>]
let ``open generic static class`` () =
    test <@ C.eval "open System.Collections.Generic.Comparer[int32] in get_Default()" = System.Collections.Generic.Comparer<int>.Default @>

    