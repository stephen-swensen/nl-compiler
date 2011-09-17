﻿module WhileLoopTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic
module C = Compiler

[<Fact>]
let ``break not allowed outside of while loop`` () =
    raises<SemanticErrorException> <@ C.eval "break()" @>

[<Fact>]
let ``continue not allowed outside of while loop`` () =
    raises<SemanticErrorException> <@ C.eval "continue()" @>

[<Fact>]
let ``simple while loop`` () =
    test <@ C.eval "x=0 in (while x<5 do x<-x+1); x " = 5 @>
