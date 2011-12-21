module Tests.EvalTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``single expression is equivalent to single statement`` options =
    test <@ evalWith options "3" = evalWith options "3;;" @>

[<Theory(Skip="not sure if we even want to allow this");EvalData>]
let ``returns last value of several statements`` options =
    test <@ evalWith options "1;;2;;3;;4;;" = 4 @>

[<Theory;NliData>]
let ``eval throws EvaluationException when errors found`` options =
    raises<EvaluationException> <@ Nli(options).Submit("INVALID") @>