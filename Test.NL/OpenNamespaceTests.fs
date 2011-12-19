module Tests.OpenNamespaceTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic

module C = Compilation

[<Theory;EvalData>]
let ``system open by default`` options =
    test <@ C.evalWith options "string('c',3)" = "ccc" @>

[<Theory;EvalData>]
let ``system+collections+generic open by default`` options =
    test <@ C.evalWith<obj> options "list[int32]()" :? System.Collections.Generic.List<int32> @>

[<Theory;EvalData>]
let ``open expression`` options =
    test <@ C.evalWith<obj> options "open System.Diagnostics in Stopwatch()" :? System.Diagnostics.Stopwatch @>

[<Theory;EvalData>]
let ``connot resolve namespace`` options =
    raisesWith 
        <@ C.evalWith options "open hello.world in ()" @>
        (expectedErrors [|18|])

