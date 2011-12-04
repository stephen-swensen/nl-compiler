module Tests.IdentifierTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``short`` () =
    let ident = Ast.Identifier("short")
    test <@ ident.Full = "short" @>
    test <@ ident.IsShort = true @>
    test <@ ident.LongPrefix = "" @>
    test <@ ident.ShortSuffix = "short" @>

[<Fact>]
let ``long`` () =
    let ident = Ast.Identifier("one.two.three.four")
    test <@ ident.Full = "one.two.three.four" @>
    test <@ ident.IsShort = false @>
    test <@ ident.LongPrefix = "one.two.three" @>
    test <@ ident.ShortSuffix = "four" @>

[<Fact>]
let ``cant be null`` () =
    raises<System.ArgumentException> <@ Ast.Identifier(null) @>

[<Fact>]
let ``cant be empty`` () =
    raises<System.ArgumentException> <@ Ast.Identifier("") @>