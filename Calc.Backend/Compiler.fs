module Swensen.Calc.Compiler

open Microsoft.FSharp.Text.Lexing
open System

open Ast
open Lexer
open Parser

open  System.Reflection.Emit

type CoreOps =
    static member Factorial(n:int) =
        let rec fact n = 
            match n with 
            | 1 | 0 -> 1
            | n -> n * fact (n-1)
        fact n


let parseFromString code =
    let lexbuff = LexBuffer<char>.FromString(code)
    let ast = Parser.start Lexer.tokenize lexbuff
    ast

let emitOpCodes (il:ILGenerator) ast =
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
            emit lenv x ; emit lenv y
            match op with
            | Plus -> il.Emit(OpCodes.Add)
            | Minus -> il.Emit(OpCodes.Sub)
            | Times -> il.Emit(OpCodes.Mul)
            | Div -> il.Emit(OpCodes.Div)
            | Pow -> 
                let meth = typeof<System.Math>.GetMethod("Pow",[|typeof<float>;typeof<float>|])
                il.Emit(OpCodes.Call, meth)
        | Fact(n,_) ->
            emit lenv n
            let meth = typeof<CoreOps>.GetMethod("Factorial",[|typeof<int>|])
            il.Emit(OpCodes.Call, meth)
        | Let(id, assign, body,ty) ->
            let local = il.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
            emit lenv assign
            il.Emit(OpCodes.Stloc, local)
            emit (Map.add id local lenv) body
        | Var(id, ty) ->
            let local = lenv |> Map.find id
            il.Emit(OpCodes.Ldloc, local)
        | Coerce(x,ty) ->
            emit lenv x
            if ty = typeof<float> then
                il.Emit(OpCodes.Conv_R8)
            elif ty = typeof<int> then
                il.Emit(OpCodes.Conv_I4)
            else
                failwithf "unsupported coersion: %A" ty
            
        | _ -> failwithf "not implemented: %A" ast

    emit Map.empty ast |> ignore

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