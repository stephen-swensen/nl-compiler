﻿///Test lex unrecognized error recovery as well as comments and newline / white space handling
module Tests.LexTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open System
open Evaluation

[<Theory;EvalData>]
let ``white space and newlines are insignificant`` options =
    test <@ evalWith options "x        = \n 3   \r\n     in x" = evalWith options "x = 3 in x" @>

[<Theory;EvalData>]
let ``error recovery in the presence of unrecognized charactors`` options =
    raisesWith <@ evalWith options "x =Ð3 inßx + y" @>
        (expectedErrors [|39;39;5|])

[<Theory;EvalData>]
let ``issue 55: should be able to subtract two ints with no spaces without parse error`` options =
    test <@ evalWith options "3-3" = 0 @>

[<Theory;EvalData>]
let ``verbatim identifier`` options =
    test <@ evalWith options "type[system.@type]" = typeof<System.Type> @>