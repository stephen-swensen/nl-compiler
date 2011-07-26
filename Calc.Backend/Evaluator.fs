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
    let dm = System.Reflection.Emit.DynamicMethod("eval", typeof<obj>, null)
    let il = dm.GetILGenerator()

    let rec emit exp =
        match exp with
        | Value(Integer x) -> 
            il.Emit(OpCodes.Ldc_I4, x)
            typeof<int>
        | Value(Rational x) -> 
            il.Emit(OpCodes.Ldc_R8, x)
            typeof<float>
        | UMinus(x) -> 
            let ty = emit x
            il.Emit(OpCodes.Neg)
            ty
        | Plus(x,y) -> callCoercedBinop x y OpCodes.Add
        | Minus(x,y) -> callCoercedBinop x y OpCodes.Sub
        | Div(x,y) -> callCoercedBinop x y OpCodes.Div
        | Times(x,y) -> callCoercedBinop x y OpCodes.Mul
//        | Pow(x,y) -> callCoercedBinop x y ( integerPow ) ( ** )
//        | Fact(n) -> 
//            match eval n with
//            | Integer(n) ->
//                let rec fact n = 
//                    if n=0 then 1 else n * fact(n - 1)
//                Integer(fact n)
//            | _ -> failwith "factorial is only valid on integers"
    and callCoercedBinop x y oc =
        let ty = 
            if emit x = typeof<float> then
                if emit y = typeof<int> then
                    il.Emit(OpCodes.Conv_R8)
                else ()
                typeof<float>
            elif emit y = typeof<float> then
                il.Emit(OpCodes.Conv_R8)
                typeof<float>
            else
                typeof<int>

        il.Emit(oc)
        ty
    
    let lexbuff = LexBuffer<char>.FromString(str)
    let exp = Parser.start Lexer.tokenize lexbuff
    
    let retTy = emit exp

    il.Emit(OpCodes.Box, retTy)
    il.Emit(OpCodes.Ret)    

    let d = dm.CreateDelegate(typeof<System.Func<obj>>) :?> System.Func<obj>

    match d.Invoke() with
    | :? float as x -> Rational(x)
    | :? int as x -> Integer(x)