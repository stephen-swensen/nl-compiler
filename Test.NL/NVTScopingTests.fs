module Tests.NVTScopingTests

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

