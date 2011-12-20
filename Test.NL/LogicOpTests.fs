module Tests.LogicOpTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open Evaluation

[<Theory;EvalData>]
let ``lhs of and  must be bool`` options =
    raisesWith 
        <@ evalWith options "3 && true" @>
        (expectedErrors [|6|])

[<Theory;EvalData>]
let ``rhs of and  must be bool`` options =
    raisesWith 
        <@ evalWith options "true && 3" @>
        (expectedErrors [|6|])

[<Theory;EvalData>]
let ``true and true`` options =
    test <@ evalWith options "true && true" = true @>

[<Theory;EvalData>]
let ``true and false`` options =
    test <@ evalWith options "true && false" = false @>

[<Theory;EvalData>]
let ``false and true`` options =
    test <@ evalWith options "false && true" = false @>

[<Theory;EvalData>]
let ``false and false`` options =
    test <@ evalWith options "false && false" = false @>

[<Theory;EvalData>]
let ``lhs of or  must be bool`` options =
    raisesWith 
        <@ evalWith options "3 || true" @>
        (expectedErrors [|6|])

[<Theory;EvalData>]
let ``rhs of or  must be bool`` options =
    raisesWith 
        <@ evalWith options "true || 6" @>
        (expectedErrors [|6|])

[<Theory;EvalData>]
let ``true or true`` options =
    test <@ evalWith options "true || true" = true @>

[<Theory;EvalData>]
let ``true or false`` options =
    test <@ evalWith options "true || false" = true @>

[<Theory;EvalData>]
let ``false or true`` options =
    test <@ evalWith options "false || true" = true @>

[<Theory;EvalData>]
let ``false or false`` options =
    test <@ evalWith options "false || false" = false @>

//[<Theory;EvalData>]
//let ``lhs of xor  must be bool`` options =
//    raisesWith 
//        <@ evalWith options "3 xor true" @>
//        (expectedErrors [|6|])

//[<Theory;EvalData>]
//let ``rhs of xor  must be bool`` options =
//    raisesWith 
//        <@ evalWith options "true xor 3" @>
//        (expectedErrors [|6|]) //double reporting error since synthetic ifthenelse constructions in rexpr.
//
//[<Theory;EvalData>]
//let ``true xor true`` options =
//    test <@ evalWith options "true xor true" = false @>
//
//[<Theory;EvalData>]
//let ``true xor false`` options =
//    test <@ evalWith options "true xor false" = true @>
//
//[<Theory;EvalData>]
//let ``false xor true`` options =
//    test <@ evalWith options "false xor true" = true @>
//
//[<Theory;EvalData>]
//let ``false xor false`` options =
//    test <@ evalWith options "false xor false" = false @>

//[<Theory;EvalData>]
//let ``case insensitive keywords`` options =
//    test <@ evalWith options "fALse XOR False" = false @>

[<Theory;EvalData>]
let ``not true`` options =
    test <@ evalWith options "!true" = false @>

[<Theory;EvalData>]
let ``not not true`` options =
    test <@ evalWith options "!!true" = true @>

[<Theory;EvalData>]
let ``not false`` options =
    test <@ evalWith options "!false" = true @>

[<Theory;EvalData>]
let ``not not false`` options =
    test <@ evalWith options "!!false" = false @>

[<Theory;EvalData>]
let ``logical not type must be boolean`` options =
    raisesWith 
        <@ evalWith options "!'c'" @>
        (expectedErrors [|6|])