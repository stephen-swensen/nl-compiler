module Tests.CastTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

open Evaluation

[<Theory;EvalData>]
let ``box primitive value type`` options =
    test <@ evalWith options "32[object]" = box 32 @>

[<Theory;EvalData>]
let ``box and ubox value type`` options =
    test <@ evalWith options "32[object][int32]" = 32 @>

[<Theory;EvalData>]
let ``down cast ref type`` options =
    test <@ evalWith options "'c'[object]" = box 'c' @>

[<Theory;EvalData>]
let ``downcast and updown cast ref type`` options =
    test <@ evalWith options "'c'[object][char]" = 'c' @>

[<Theory;EvalData>]
let ``downcast and upcast ref type to and from interface`` options =
    test <@ evalWith<obj> options "open system.collections in arraylist()[ienumerable][arraylist]" |> ignore; true @>

[<Theory;EvalData>]
let ``downcast and upcast value type to and from interface`` options =
    test <@ evalWith<obj> options "biginteger()[IComparable][biginteger]" |> ignore; true @>

[<Theory;EvalData>]
let ``semantic error trying to cast sealed value type to anything other than object or implemented interface `` options =
    raisesWith 
        <@ evalWith options "32[string]" @>
        (expectedErrors [|22|])

[<Theory;EvalData>]
let ``semantic error trying to cast sealed ref type to anything other than object or implemented interface `` options =
    raisesWith 
        <@ evalWith options "\"asdf\"[char]" @>
        (expectedErrors [|22|])

[<Theory;EvalData>] //fixed
let ``cast var - does not needs to be surrounded with parens`` options =
    test <@ evalWith options "x = 3 in x[object][int32]" = 3 @>

[<Theory;EvalData>]
let ``cast var has more than one generic ty arg`` options =
    raisesWith 
        <@ evalWith options "x = 3 in x[object,int32]" @>
        (expectedErrors [|53|])

[<Theory;EvalData>]
let ``cannot cast value to its own type`` options =
    raisesWith 
        <@ evalWith options "3[int32]" @>
        (expectedErrors [|21|])

[<Theory;EvalData>]
let ``cast int to double, a widening coersion`` options =
    test <@ evalWith options "3[double]" = 3.0 @>

[<Theory;EvalData>]
let ``cast double to int, a narrowing coersion`` options =
    test <@ evalWith options "3.0[int32]" = 3 @>

[<Theory;EvalData>]
let ``cast int32 to biginteger, a biginteger op_implicit `` options =
    test <@ evalWith options "3[biginteger]" = 3I @>

[<Theory;EvalData>]
let ``cast biginteger to int32, a biginteger op_explicit `` options =
    test <@ evalWith options "biginteger()[int32]" = 0 @>

[<Theory;EvalData>]
let ``casting to Void will always fail`` options =
    raisesWith 
        <@ evalWith options "3[object][System.Void]" @>
        (expectedErrors [|20|])

[<Theory;EvalData>]
let ``could not resolve cast type`` options =
    raisesWith 
        <@ evalWith options "3[invalid]" @>
        (expectedErrors [|1|])

[<Theory;EvalData>]
let ``cast char to int32, an implicit coersion`` options =
    test <@ evalWith options "'A'[int32]" = 65 @>

[<Theory(Skip="todo");EvalData>]
let ``issue 52: cast uint32 maxvalue to int64: should be no loss of precision`` options =
    test <@ evalWith options "uint32.parse(\"4294967295\")[int64]" = 4294967295L @>
