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
        | String(x,_) -> 
            il.Emit(OpCodes.Ldstr, x)
        | UMinus(x,_) -> 
            emit lenv x
            il.Emit(OpCodes.Neg)
        | Binop(op,x,y,_) -> 
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
        | Let(id, assign, body,_) ->
            let local = il.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
            emit lenv assign
            il.Emit(OpCodes.Stloc, local)
            emit (Map.add id local lenv) body
        | Var(id, _) ->
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
        | StaticCall(meth,args,_) ->
            args |> List.iter (emit lenv)
            il.Emit(OpCodes.Call, meth)
        | InstanceCall(instance,meth,args,_) ->
            emit lenv instance
            if instance.Type.IsValueType then
                let loc = il.DeclareLocal(instance.Type)
                il.Emit(OpCodes.Stloc, loc)
                il.Emit(OpCodes.Ldloca, loc)
            
            for arg in args do emit lenv arg
            
            if instance.Type.IsValueType then
                il.Emit(OpCodes.Call, meth)
            else
                il.Emit(OpCodes.Callvirt, meth)
            
        //| _ -> failwithf "not implemented: %A" ast

    emit Map.empty ast |> ignore

let dmFromAst (ast:exp) =
    let dm = System.Reflection.Emit.DynamicMethod("Calc", ast.Type, null)
    let il = dm.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Ret)
    dm

let dmFromString code =
    (parseFromString>>dmFromAst) code

let eval code = (dmFromString code).Invoke(null,null)