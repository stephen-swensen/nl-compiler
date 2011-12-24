module Tests.EvalTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``single expression is equivalent to single statement`` options =
    test <@ evalWith options "3" = evalWith options "3;;" @>

[<Theory;EvalData>]
let ``eval cannot handle several statements`` options =
    raisesWith 
        <@ evalWith options "1;;2;;3;;4;;" = 4 @>
        (expectedErrors [|36|])

[<Theory;EvalData>]
let ``eval throws EvaluationException when errors found`` options =
    raises<EvaluationException> <@ evalWith options "INVALID" @>