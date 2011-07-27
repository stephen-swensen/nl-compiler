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
        | Value((Integer _), ty) -> ty := typeof<int>
        | Value((Rational _), ty) -> ty := typeof<float>
        | Div(x,y,ty) | Times(x,y,ty) | Plus(x,y,ty) | Minus(x,y,ty) | Pow(x,y,ty) ->
            fillTys x ; fillTys y
            if x.Type = typeof<float> || y.Type = typeof<float> then
                ty := typeof<float>
            else
                ty := typeof<int>
        | Fact x -> 
            fillTys x
        | UMinus (x,ty) ->
            fillTys x
            ty := x.Type

    fillTys ast
    ast

let emitOpCodes (il:ILGenerator) ast =
    let rec emit ast =
        match ast with
        | Value(Integer x,_) -> 
            il.Emit(OpCodes.Ldc_I4)
        | Value(Rational x,_) -> 
            il.Emit(OpCodes.Ldc_R8)
        | UMinus(x,_) -> 
            emit x
            il.Emit(OpCodes.Neg)
        | Plus(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Add
        | Minus(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Sub
        | Div(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Div
        | Times(x,y,ty) -> emitCoercedBinop x y !ty OpCodes.Mul
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
    il

let delegateFromAst (ast:exp) =
    let dm = System.Reflection.Emit.DynamicMethod("eval", typeof<obj>, null)
    let il = dm.GetILGenerator()
    il.Emit(OpCodes.Box, ast.Type)
    il.Emit(OpCodes.Ret)
    dm.CreateDelegate(typeof<System.Func<obj>>) :?> System.Func<obj>

let delegateFromString code =
    (parseFromString>>delegateFromAst) code