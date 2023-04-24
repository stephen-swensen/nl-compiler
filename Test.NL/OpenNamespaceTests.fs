module Tests.OpenNamespaceTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

open Evaluation

[<Theory;EvalData>]
let ``system open by default`` options =
    test <@ evalWith options "string('c',3)" = "ccc" @>

[<Theory;EvalData>]
let ``system+collections+generic open by default`` options =
    test <@ evalWith<obj> options "list[int32]()" :? System.Collections.Generic.List<int32> @>

[<Theory;EvalData>]
let ``open expression`` options =
    test <@ evalWith<obj> options "open System.Diagnostics in Stopwatch()" :? System.Diagnostics.Stopwatch @>

[<Theory;EvalData>]
let ``connot resolve namespace`` options =
    raisesWith 
        <@ evalWith options "open hello.world in ()" @>
        (expectedErrors [|18|])

