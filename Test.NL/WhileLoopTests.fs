module Tests.WhileLoopTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
module C = Compilation

[<Fact>]
let ``break not allowed outside of while loop`` () =
    raisesWith 
        <@ C.eval "break()" @>
        (expectedErrors [|8|])

[<Fact>]
let ``continue not allowed outside of while loop`` () =
    raisesWith 
        <@ C.eval "continue()" @>
        (expectedErrors [|9|])

[<Fact>]
let ``simple while loop`` () =
    test <@ C.eval "x=0 in while x<5 { x<-x+1 }; x" = 5 @>

[<Fact>]
let ``while loop with break`` () =
    test <@ C.eval "x=0 in while x<5 { x<-x+1; if x == 3 { break() } }; x" = 3 @>

[<Fact>]
let ``return value is void so can't assign`` () =
    raisesWith 
        <@ C.eval "y = x = 0 in while x<5 { x<-x+1 } in ()" @>
        (expectedErrors [|16|])

[<Fact>]
let ``continue`` () =
    test <@ C.eval "x=0 in y=0 in while x<5 { x<-x+1; if x == 3 { continue() } else { y<-y+1 } }; y " = 4 @>

[<Fact>]
let ``nested`` () =
    test <@ C.eval "x=0 in y=0 in while x<5 { x<-x+1; z=0 in while z<5 { z<-z+1; y<-y+1 } }; y " = 25 @>

[<Fact>]
let ``nested breaks`` () =
    test <@ C.eval "x = 0 in while true { while true { break() }; break() }; x" = 0 @>

[<Fact>]
let ``nested continue`` () =
    test <@ C.eval "y=0 in while true { x=0 in while x<5 { x<-x+1; if x == 3 { continue() } else { y<-y+1 } } ; break() }; y" = 4 @>

[<Fact>]
let ``unreachable break error`` () =
    raisesWith 
        <@ C.eval "while false { break(); () }" @>
        (expectedErrors [|17|])

[<Fact>]
let ``unreachable continue error`` () =
    raisesWith 
        <@ C.eval "while false { continue(); () }" @>
        (expectedErrors [|17|])

[<Fact>]
let ``condition is not boolean error`` () =
    raisesWith 
        <@ C.eval "while 'c' { () }" @>
        (expectedErrors [|6|])