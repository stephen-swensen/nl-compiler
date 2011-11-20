module Tests.OpenNamespaceTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic

module C = Compilation

[<Fact>]
let ``system open by default`` () =
    test <@ C.eval "string('c',3)" = "ccc" @>

[<Fact>]
let ``system.collections open by default`` () =
    test <@ C.eval<obj> "arraylist()" :? System.Collections.ArrayList @>

[<Fact>]
let ``open expression`` () =
    test <@ C.eval<obj> "open System.Diagnostics in Stopwatch()" :? System.Diagnostics.Stopwatch @>

[<Fact>]
let ``connot resolve namespace`` () =
    raisesWith 
        <@ C.eval "open hello.world in ()" @>
        (expectedErrors [|18|])

