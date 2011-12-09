module Tests.PathTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
open Ast
module C = Compilation

[<Fact>]
let ``short`` () =
    let path = Path("short")
    test <@ path.Text = "short" @>
    test <@ path.IsShort = true @>
    test <@ path.LongPrefix = "" @>
    test <@ path.ShortSuffix = "short" @>

[<Fact>]
let ``long`` () =
    let path = Path("one.two.three.four")
    test <@ path.Text = "one.two.three.four" @>
    test <@ path.IsShort = false @>
    test <@ path.LongPrefix = "one.two.three" @>
    test <@ path.ShortSuffix = "four" @>

[<Fact>]
let ``cant be null`` () =
    raises<System.ArgumentException> <@ Path(null) @>

[<Fact>]
let ``cant be empty`` () =
    raises<System.ArgumentException> <@ Path("") @>