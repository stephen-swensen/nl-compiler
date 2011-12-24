module Tests.WhileLoopTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``break not allowed outside of while loop`` options =
    raisesWith 
        <@ evalWith options "break()" @>
        (expectedErrors [|8|])

[<Theory;EvalData>]
let ``continue not allowed outside of while loop`` options =
    raisesWith 
        <@ evalWith options "continue()" @>
        (expectedErrors [|9|])

[<Theory;EvalData>]
let ``simple while loop`` options =
    test <@ evalWith options "x=0 in while x<5 { x<-x+1 }; x" = 5 @>

[<Theory;EvalData>]
let ``while loop with break`` options =
    test <@ evalWith options "x=0 in while x<5 { x<-x+1; if x == 3 { break() } }; x" = 3 @>

[<Theory;EvalData>]
let ``return value is void so can't assign`` options =
    raisesWith 
        <@ evalWith options "y = x = 0 in while x<5 { x<-x+1 } in ()" @>
        (expectedErrors [|16|])

[<Theory;EvalData>]
let ``continue`` options =
    test <@ evalWith options "x=0 in y=0 in while x<5 { x<-x+1; if x == 3 { continue() } else { y<-y+1 } }; y " = 4 @>

[<Theory;EvalData>]
let ``nested`` options =
    test <@ evalWith options "x=0 in y=0 in while x<5 { x<-x+1; z=0 in while z<5 { z<-z+1; y<-y+1 } }; y " = 25 @>

[<Theory;EvalData>]
let ``nested breaks`` options =
    test <@ evalWith options "x = 0 in while true { while true { break() }; break() }; x" = 0 @>

[<Theory;EvalData>]
let ``nested continue`` options =
    test <@ evalWith options "y=0 in while true { x=0 in while x<5 { x<-x+1; if x == 3 { continue() } else { y<-y+1 } } ; break() }; y" = 4 @>

[<Theory;EvalData>]
let ``unreachable break error`` options =
    raisesWith 
        <@ evalWith options "while false { break(); () }" @>
        (expectedErrors [|17|])

[<Theory;EvalData>]
let ``unreachable continue error`` options =
    raisesWith 
        <@ evalWith options "while false { continue(); () }" @>
        (expectedErrors [|17|])

[<Theory;EvalData>]
let ``condition is not boolean error`` options =
    raisesWith 
        <@ evalWith options "while 'c' { () }" @>
        (expectedErrors [|6|])

[<Theory;EvalData>]
let ``while loop with non void body discards value from the IL stack`` options =
    test <@ evalWith options "x=0 in while x<5 { x<-x+1; 3 }; x" = 5 @>