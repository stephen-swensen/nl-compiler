module Swensen.NewLang.Compiler

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

let emitOpCodes (il:ILGenerator) ast =
    let rec emit lenv ast =
        match ast with
        | Int32 x  -> il.Emit(OpCodes.Ldc_I4, x)
        | Double x -> il.Emit(OpCodes.Ldc_R8, x)
        | String x -> il.Emit(OpCodes.Ldstr, x)
        | UMinus(x,_) -> 
            emit lenv x
            il.Emit(OpCodes.Neg)
        | NumericBinop(op,x,y,_) -> 
            emit lenv x ; emit lenv y
            match op with
            | Plus  -> il.Emit(OpCodes.Add)
            | Minus -> il.Emit(OpCodes.Sub)
            | Times -> il.Emit(OpCodes.Mul)
            | Div   -> il.Emit(OpCodes.Div)
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
                failwithf "unsupported coersion: %A" ty //shouldn't be possible since already ty checked
        | StaticCall(meth,args,_) ->
            args |> List.iter (emit lenv)
            il.Emit(OpCodes.Call, meth)
        | InstanceCall(instance,meth,args,_) ->
            emit lenv instance
            if instance.Type.IsValueType then
                let loc = il.DeclareLocal(instance.Type)
                il.Emit(OpCodes.Stloc, loc)
                il.Emit(OpCodes.Ldloca, loc)
            
            for arg in args do 
                emit lenv arg
            
            if instance.Type.IsValueType then
                il.Emit(OpCodes.Call, meth)
            else
                il.Emit(OpCodes.Callvirt, meth)
    emit Map.empty ast |> ignore

let dmFromAst (ast:exp) =
    let dm = System.Reflection.Emit.DynamicMethod("NewLang", ast.Type, null)
    let il = dm.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Ret)
    dm

let dmFromString code =
    (parseFromString>>dmFromAst) code

let eval code = (dmFromString code).Invoke(null,null)

open System.Reflection

let compile ast asmName asmFileName =
    let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndSave)
    let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".netmodule")
    let tyBuilder = modBuilder.DefineType(asmName + ".Application", TypeAttributes.Public)
    let methBuilder = tyBuilder.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Static, typeof<System.Void>, null)
    
    let il = methBuilder.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Ret)

    asmBuilder.SetEntryPoint(methBuilder)
    asmBuilder.Save(asmFileName)