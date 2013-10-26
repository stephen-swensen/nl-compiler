module Tests.EvalTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``statements not valid in eval`` options =
    raisesWith 
        <@ evalWith options "1;;2;;3;;4;;" = 4 @>
        (expectedErrors [|53|])

[<Theory;EvalData>]
let ``eval throws EvaluationException when errors found`` options =
    raises<EvaluationException> <@ evalWith options "INVALID" @>

[<Theory;NliData>]
let ``prelude`` options =
    let nli = new Nli()
    test <@ evalWith options ("""eval[int32]("3 + 2 + eval[int32](\"1\")")""") = 6 @>