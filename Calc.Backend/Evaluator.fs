module Swensen.Calc.Evaluator
open Microsoft.FSharp.Text.Lexing
open System

open Ast
open Lexer
open Parser

let (|Box|) x = box x
let (|Float|) x = float x

let integerPow x n = 
    int ((x |> float) ** (n |> float))

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
        | Pow(x,y) -> callCoercedBinop x y ( integerPow ) ( ** )
        | Fact(n) -> 
            match eval n with
            | Integer(n) ->
                let rec fact n = 
                    if n=0 then 1 else n * fact(n - 1)
                Integer(fact n)
            | _ -> failwith "factorial is only valid on integers"
    and callCoercedBinop x y onInteger onRational =
        match eval x, eval y with
        | Integer x, Integer y -> Integer(onInteger x y)
        | (Rational(x) | Integer(Float(x))), (Integer(Float(y)) | Rational(y)) -> Rational(onRational x y)
    
    let lexbuff = LexBuffer<char>.FromString(str)
    let exp = Parser.start Lexer.tokenize lexbuff
    eval exp

open System.Reflection.Emit

let eval_il str =
    let dm = System.Reflection.Emit.DynamicMethod("eval", typeof<int>, null)
    let il = dm.GetILGenerator()

    let rec emit exp =
        match exp with
        | Value(Integer x) -> 
            let code = OpCodes.Ldc_I4
            Console.WriteLine code
            il.Emit(code, x)
        | Value(Rational x) -> 
            let code = OpCodes.Ldc_R8
            Console.WriteLine code
            il.Emit(code, x)
        | UMinus(x) -> 
            emit x; 
            let code = OpCodes.Neg
            Console.WriteLine code
            il.Emit(code)
//        | Plus(x,y) -> callCoercedBinop x y (+) (+)
//        | Minus(x,y) -> callCoercedBinop x y (-) (-) 
//        | Div(x,y) -> callCoercedBinop x y (/) (/) 
//        | Times(x,y) -> callCoercedBinop x y (*) (*)
//        | Pow(x,y) -> callCoercedBinop x y ( integerPow ) ( ** )
//        | Fact(n) -> 
//            match eval n with
//            | Integer(n) ->
//                let rec fact n = 
//                    if n=0 then 1 else n * fact(n - 1)
//                Integer(fact n)
//            | _ -> failwith "factorial is only valid on integers"
//    and callCoercedBinop x y onInteger onRational =
//        match eval x, eval y with
//        | Integer x, Integer y -> Integer(onInteger x y)
//        | (Rational(x) | Integer(Float(x))), (Integer(Float(y)) | Rational(y)) -> Rational(onRational x y)
    
    let lexbuff = LexBuffer<char>.FromString(str)
    let exp = Parser.start Lexer.tokenize lexbuff
    
    emit exp |> ignore
    
//    let code = OpCodes.Box
//    Console.WriteLine code
//    il.Emit(code)
    
    let code = OpCodes.Ret
    Console.WriteLine code
    il.Emit(code)    

    let d = dm.CreateDelegate(typeof<System.Func<int>>) :?> System.Func<int>
    Integer(d.Invoke())
    
//    match d.Invoke() with
//    | :? float as x -> Rational(x)
//    | :? int as x -> Integer(x)