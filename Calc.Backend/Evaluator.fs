module Swensen.Calc.Evaluator
open Microsoft.FSharp.Text.Lexing
open System

open Ast
open Lexer
open Parser

let (|Box|) x = box x
let (|Float|) x = float x

let eval str =
    let rec eval exp =
        match exp with
        | Value x -> x
        | UMinus(x) -> 
            match eval x with
            | Integer x -> Integer(-x)
            | Rational x -> Rational(-x)
        | Plus(x,y) -> callCoercedBinop x y (+) (+)
        | Minus(x,y) -> callCoercedBinop x y (-) (-) 
        | Div(x,y) -> callCoercedBinop x y (/) (/) 
        | Times(x,y) -> callCoercedBinop x y (*) (*)
        | Pow(x,y) -> callCoercedBinop x y ( ** ) ( ** )
        | Fact(n) -> 
            match eval n with
            | Integer(n) ->
                let rec fact n = 
                    if n=0 then 1 else n * fact(n - 1)
                Integer(fact n)
            | _ -> failwith "factorial is only valid on integers"
    and callCoercedBinop x y onInteger onRational =
        match eval x, eval y with
        | Integer x, Integer y -> Integer(x + y)
        | (Rational(x) | Integer(Float(x))), (Integer(Float(y)) | Rational(y)) -> Rational(x + y)
    
    let lexbuff = LexBuffer<char>.FromString(str)
    let exp = Parser.start Lexer.tokenize lexbuff
    eval exp