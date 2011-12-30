module Tests.NliTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

[<Theory;NliData>]
let ``single expression is equivalent to single Do statement`` options =
    test <@ Nli(options).Submit("3") = Nli(options).Submit("3;;") @>

[<Theory;NliData>]
let ``issue 45: double semilcolon should not reduced to single semicolon in let bindings`` options =
    test <@ Nli(options).Submit("x = 3 in x;;") = [|("it0", 3 :> obj, typeof<int32>)|] @>

[<Theory;NliData>]
let ``single Do statement is bound to 0th it`` options =
    test <@ Nli(options).Submit("3") = [|("it0", 3 :> obj, typeof<int32>)|] @>

[<Theory;NliData>]
let ``let statement`` options =
    test <@ Nli(options).Submit("x = 3;;") = [|("x", 3 :> obj, typeof<int32>)|] @>

[<Theory;NliData>]
let ``let and do statements intersparsed`` options =
    test <@ Nli(options).Submit("x = 1;;2;;3;;y=4;;5;;") 
             = [|("x", 1 :> obj, typeof<int32>);("it0", 2 :> obj, typeof<int32>);("it1", 3 :> obj, typeof<int32>);("y", 4 :> obj, typeof<int32>);("it2", 5 :> obj, typeof<int32>)|] @>

[<Theory;NliData>]
let ``Submit throws NliException when errors found`` options =
    raises<NliException> <@ Nli(options).Submit("INVALID") @>

[<Theory;NliData>]
let ``can reference variables from previous submits`` options =
    test <@ 
            let nli = Nli(options)
            nli.Submit("x = 3;;") |> ignore 
            nli.Submit("y = x + 2;;") = [|("y", 5 :> obj, typeof<int32>)|]
         @>