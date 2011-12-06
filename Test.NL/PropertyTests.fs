module Tests.PropertyTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``get static property`` () =
    test <@ C.eval "system.datetime.now" < (DateTime.Now.AddDays(1.0)) @>