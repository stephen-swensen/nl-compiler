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
    ast

let emitOpCodesWithEnvs lenv (il:ILGenerator) ast =
    let rec emit lenv ast =
        match ast with
        | Int32(x,_) -> 
            il.Emit(OpCodes.Ldc_I4, x)
        | Double(x,_) -> 
            il.Emit(OpCodes.Ldc_R8, x)
        | UMinus(x,_) -> 
            emit lenv x
            il.Emit(OpCodes.Neg)
        | Binop(op,x,y,ty) -> 
            if ty = typeof<int> then
                emit lenv x ; emit lenv y
            else //float
                emit lenv x
                if x.Type = typeof<int> then
                    il.Emit(OpCodes.Conv_R8)

                emit lenv y
                if y.Type = typeof<int> then
                    il.Emit(OpCodes.Conv_R8)

            let ilop =
                match op with
                | Plus -> OpCodes.Add
                | Minus -> OpCodes.Sub
                | Times -> OpCodes.Mul
                | Div -> OpCodes.Div
                | Pow -> failwith "pow not implemented"

            il.Emit(ilop)
        | Let(id, assign, body,ty) ->
            let local = il.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
            emit lenv assign
            il.Emit(OpCodes.Stloc, local)
            emit (Map.add id local lenv) body
        | Var(id, ty) ->
            let local = lenv |> Map.find id
            il.Emit(OpCodes.Ldloc, local)
        | _ -> failwithf "not implemented: %A" ast

    emit lenv ast |> ignore

let emitOpCodes = emitOpCodesWithEnvs Map.empty

let delegateFromAst ast =
    let dm = System.Reflection.Emit.DynamicMethod("", typeof<obj>, null)
    let il = dm.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Box, ast.Type)
    il.Emit(OpCodes.Ret)
    dm.CreateDelegate(typeof<System.Func<obj>>) :?> System.Func<obj>

let delegateFromString code =
    (parseFromString>>delegateFromAst) code

let eval code = (delegateFromString code).Invoke()