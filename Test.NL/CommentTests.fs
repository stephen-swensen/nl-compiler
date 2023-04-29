module Tests.Comments

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open System
open Evaluation

[<Fact>]
let ``single line comment with newline and eol breaks`` () =
    let code = """//hello //world yay!
3
//eol example 5"""
    test <@ eval code = 3 @>

[<Fact>]
let ``simple multi line comment`` () =
    let code = """/*
3
*/5"""
    test <@ eval code = 5 @>

[<Fact>]
let ``eof multi line comment`` () =
    let code = """5/*
3
"""
    test <@ eval code = 5 @>

[<Fact>]
let ``nested multi line comment`` () =
    let code = """5/*
//hello world!
/* 3 */
*/"""
    test <@ eval code = 5 @>