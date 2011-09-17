module Tests.SequentialTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compiler

[<Fact>]
let ``sequential expression`` () =
    test <@ C.eval "3;4" = 4 @>

[<Fact>]
let ``multiple sequential expressions`` () =
    test <@ C.eval "3;4;5" = 5 @>

[<Fact>]
let ``sequential expressions have weak right associativity`` () =
    test <@ C.eval "2 + 3 ; 3 + 5" = 8 @>

[<Fact>]
let ``sequential expression with rhs void (result does not need to be popped from the stack)`` () =
    test <@ C.eval "system.console.writeline(\"3\"); 4" = 4 @>