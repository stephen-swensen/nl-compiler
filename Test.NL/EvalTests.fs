module Tests.EvalTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``single statement is equivalent to single statement`` options =
    test <@ evalWith options "3" = evalWith options "3;;" @>

[<Theory(Skip="asdf");EvalData>]
let ``returns last value of several statements`` options =
    test <@ evalWith options "1;;2;;3;;4;;" = 4 @>

