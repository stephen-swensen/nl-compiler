module Tests.NliTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic

[<Theory;NliData>]
let ``single expression is equivalent to single statement`` options =
    test <@ Nli(options).Submit("3") = Nli(options).Submit("3;;") @>

[<Theory;NliData>]
let ``issue 45: double semilcolon should not reduced to single semicolon in let bindings`` options =
    test <@ Nli(options).Submit("x = 3 in x;;") = [|("it_0", 3 :> obj)|] @>

[<Theory;NliData>]
let ``single Do statement is bound to 0th it`` options =
    test <@ Nli(options).Submit("3") = [|("it_0", 3 :> obj)|] @>

[<Theory;NliData>]
let ``let statement`` options =
    test <@ Nli(options).Submit("x = 3;;") = [|("x", 3 :> obj)|] @>

[<Theory;NliData>]
let ``let and do statements intersparsed`` options =
    test <@ Nli(options).Submit("x = 1;;2;;3;;y=4;;5;;") 
             = [|("x", 1 :> obj);("it_0", 2 :> obj);("it_1", 3 :> obj);("y", 4 :> obj);("it_2", 5 :> obj)|] @>
