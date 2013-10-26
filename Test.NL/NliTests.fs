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
    test <@ Nli(options).Submit("x = 3 in x;;") = [("it0", 3 :> obj, typeof<int32>)] @>

[<Theory;NliData>]
let ``single Do statement is bound to 0th it`` options =
    test <@ Nli(options).Submit("3") = [("it0", 3 :> obj, typeof<int32>)] @>

[<Theory;NliData>]
let ``let statement`` options =
    test <@ Nli(options).Submit("x = 3;;") = [("x", 3 :> obj, typeof<int32>)] @>

[<Theory;NliData>]
let ``let and do statements intersparsed`` options =
    test <@ Nli(options).Submit("x = 1;;2;;3;;y=4;;5;;") 
             = [("x", 1 :> obj, typeof<int32>);("it0", 2 :> obj, typeof<int32>);("it1", 3 :> obj, typeof<int32>);("y", 4 :> obj, typeof<int32>);("it2", 5 :> obj, typeof<int32>)] @>

[<Theory;NliData>]
let ``Submit throws NliException when errors found`` options =
    raises<NliException> <@ Nli(options).Submit("INVALID") @>

[<Theory;NliData>]
let ``issue 57: Need to strip TypeInitializationException from source NLI exceptions`` options =
    raises<System.FormatException> <@ Nli(options).TrySubmit("int32.parse(\"x\")") @>

[<Theory;NliData>]
let ``issue 57: Need to strip TypeInitializationException from source NLI exceptions but not if geniune`` options =
    raises<System.TypeInitializationException> <@ Nli(options).TrySubmit("throw(TypeInitializationException(\"System.Exception\", exception()))") @>

[<Theory;NliData;>]
let ``issue 56: can reference variable from previous statement in same submit`` options =
    test <@ 
            Nli(options).Submit("x = 3;;x + 4;;") = [("x", 3 :> obj, typeof<int32>);("it0", 7 :> obj, typeof<int32>)]
         @>

[<Theory;NliData>]
let ``can reference variables from previous submits`` options =
    test <@ 
            let nli = Nli(options)
            nli.Submit("x = 3;;") |> ignore 
            nli.Submit("y = x + 2;;") = [("y", 5 :> obj, typeof<int32>)]
         @>

[<Fact(Skip="it seems that the vs test runner holds on to the NLI-ASSEMBLY thus foiling this test")>]
let ``Reset frees dynamic assembly for collection`` () =
    let mutable nli = Nli()
    nli.Submit("x = 3;;") |> ignore
    nli <- Nli()    
    nli.Submit("x = 4;;") |> ignore
    System.GC.Collect()
    let asmNames = System.AppDomain.CurrentDomain.GetAssemblies() |> Array.map (fun asm -> asm.FullName)
    test <@ asmNames |> Seq.filter (fun name -> name.Contains("NLI-ASSEMBLY")) |> Seq.length = 1 @>

[<Fact>]
let ``self reference in session`` () =
    let nli = new Nli()
    test <@ let nli' = nli.Variables.["nli"].Value in nli' :?> Nli = nli @>
    test <@ let [(_,nli',_)] = nli.Submit("nli") in nli' :?> Nli = nli @>
    test <@ let [(_,nli',_)] = nli.Submit("nli.Submit(\"nli\").get_Item(0).Item2") in nli' :?> Nli = nli @>

[<Theory;NliData>]
let ``add variable`` options =
    let nli = new Nli(options)
    nli.AddVariable("x", 3)
    test <@ nli.Variables.["x"].Value :?> int = 3 @>
    test <@ let [(_,res,_)] = nli.Submit("x + 1") in res :?> int = 4 @>
    nli.Submit("x <- 5") |> ignore
    test <@ nli.Variables.["x"].Value :?> int = 5 @>
