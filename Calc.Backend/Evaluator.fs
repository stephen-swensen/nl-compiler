module Swensen.Calc.Evaluator
open Microsoft.FSharp.Text.Lexing
open System

open Ast
open Lexer
open Parser

let eval str =
    let rec eval exp =
        match exp with
        | Rational x -> x
        | UMinus x -> - eval x
        | Plus(x,y) -> eval x + eval y
        | Minus(x,y) -> eval x - eval y
        | Div(x,y) -> eval x /  eval y
        | Times(x,y) -> eval x * eval y
        | Pow(x,y) -> eval x ** eval y
        | Fact(n) -> 
            let rec fact n = 
                if n=0 then 1 else n * fact(n - 1)
            fact (eval n |> int) |> float

    
    let lexbuff = LexBuffer<char>.FromString(str)
    let exp = Parser.start Lexer.tokenize lexbuff
    eval exp