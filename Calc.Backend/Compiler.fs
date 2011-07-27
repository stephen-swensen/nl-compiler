module Swensen.Calc.Compiler

open Microsoft.FSharp.Text.Lexing
open System

open Ast
open Lexer
open Parser

open  System.Reflection.Emit

let parseFromString code =
    let lexbuff = LexBuffer<char>.FromString(code)
    let ast = Parser.start Lexer.tokenize lexbuff
    let rec fillTys = function
        | Value _ -> ()
        | Div(x,y,ty) | Times(x,y,ty) | Plus(x,y,ty) | Minus(x,y,ty) | Pow(x,y,ty) ->
            fillTys x ; fillTys y
            if x.Type = typeof<float> || y.Type = typeof<float> then
                ty := typeof<float>
            else
                ty := typeof<int>
        | Fact x -> 
            fillTys x
            if x.Type <> typeof<int> then
                failwithf "invalid type in factorial (must be int but is: %A)" x.Type
        | UMinus (x,ty) ->
            fillTys x
            ty := x.Type

    fillTys ast
    ast

let emitOpCodes (il:ILGenerator) ast =
    let rec emit ast =
        match ast with
        | Value(Integer x) -> 
            il.Emit(OpCodes.Ldc_I4, x)
        | Value(Rational x) -> 
            il.Emit(OpCodes.Ldc_R8, x)
        | UMinus(x,_) -> 
            emit x
            il.Emit(OpCodes.Neg)
        | Plus(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Add
        | Minus(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Sub
        | Div(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Div
        | Times(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Mul
//        | Fact(n) -> 
//            emit n
//            for i in 2 .. n do
//                emit n

                
        | _ -> failwith "not implemented"
    and emitCoercedBinop x y ty oc =        
        if ty = typeof<int> then
            emit x ; emit y
        else //float
            emit x
            if x.Type = typeof<int> then
                il.Emit(OpCodes.Conv_R8)

            emit y
            if y.Type = typeof<int> then
                il.Emit(OpCodes.Conv_R8)

        il.Emit(oc)

    emit ast |> ignore

let delegateFromAst (ast:exp) =
    let dm = System.Reflection.Emit.DynamicMethod("", typeof<obj>, null)
    let il = dm.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Box, ast.Type)
    il.Emit(OpCodes.Ret)
    dm.CreateDelegate(typeof<System.Func<obj>>) :?> System.Func<obj>

let delegateFromString code =
    (parseFromString>>delegateFromAst) code

let eval code = (delegateFromString code).Invoke()