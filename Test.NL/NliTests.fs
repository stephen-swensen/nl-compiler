module Tests.NliTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic

[<Theory;NliData>]
let ``single expression is equivalent to single statement`` options =
    test <@ Nli(options).Submit("3") = Nli(options).Submit("3") @>

[<Theory;NliData>]
let ``single Do statement is bound to 0th it`` options =
    test <@ Nli(options).Submit("3") = [|("it_0", 3 :> obj)|] @>
