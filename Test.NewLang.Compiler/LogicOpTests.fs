﻿module LogicOpTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compiler

[<Fact>]
let ``lhs of and  must be bool`` () =
    raises<SemanticErrorException> <@ C.eval "3 and true" @>

[<Fact>]
let ``rhs of and  must be bool`` () =
    raises<SemanticErrorException> <@ C.eval "true and 3" @>

[<Fact>]
let ``true and true`` () =
    test <@ C.eval "true and true" = true @>

[<Fact>]
let ``true and false`` () =
    test <@ C.eval "true and false" = false @>

[<Fact>]
let ``false and true`` () =
    test <@ C.eval "false and true" = false @>

[<Fact>]
let ``false and false`` () =
    test <@ C.eval "false and false" = false @>

[<Fact>]
let ``lhs of or  must be bool`` () =
    raises<SemanticErrorException> <@ C.eval "3 or true" @>

[<Fact>]
let ``rhs of or  must be bool`` () =
    raises<SemanticErrorException> <@ C.eval "true or 3" @>

[<Fact>]
let ``true or true`` () =
    test <@ C.eval "true or true" = true @>

[<Fact>]
let ``true or false`` () =
    test <@ C.eval "true or false" = true @>

[<Fact>]
let ``false or true`` () =
    test <@ C.eval "false or true" = true @>

[<Fact>]
let ``false or false`` () =
    test <@ C.eval "false or false" = false @>

[<Fact>]
let ``lhs of xor  must be bool`` () =
    raises<SemanticErrorException> <@ C.eval "3 xor true" @>

[<Fact>]
let ``rhs of xor  must be bool`` () =
    raises<SemanticErrorException> <@ C.eval "true xor 3" @>

[<Fact>]
let ``true xor true`` () =
    test <@ C.eval "true xor true" = false @>

[<Fact>]
let ``true xor false`` () =
    test <@ C.eval "true xor false" = true @>

[<Fact>]
let ``false xor true`` () =
    test <@ C.eval "false xor true" = true @>

[<Fact>]
let ``false xor false`` () =
    test <@ C.eval "false xor false" = false @>

[<Fact>]
let ``case insensitive keywords`` () =
    test <@ C.eval "fALse XOR False" = false @>

[<Fact>]
let ``not true`` () =
    test <@ C.eval "~true" = false @>

[<Fact>]
let ``not not true`` () =
    test <@ C.eval "~~true" = true @>

[<Fact>]
let ``not false`` () =
    test <@ C.eval "~false" = true @>

[<Fact>]
let ``not not false`` () =
    test <@ C.eval "~~false" = false @>