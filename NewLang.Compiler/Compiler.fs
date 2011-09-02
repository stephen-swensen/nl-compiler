﻿module Swensen.NewLang.Compiler

open Microsoft.FSharp.Text.Lexing
open System
open Swensen.NewLang

open Lexer
open Parser

module SA = SemanticAnalysis

open  System.Reflection.Emit

let setInitialPos (lexbuf:LexBuffer<char>) filename = 
    lexbuf.EndPos <- { pos_bol = 0
                       pos_fname=filename
                       pos_cnum=1
                       pos_lnum=1 }

let parseFromString code =
    let lexbuf = LexBuffer<char>.FromString(code)
    setInitialPos lexbuf ""
    try 
        Parser.start Lexer.tokenize lexbuf
        |> SA.tycheck ["system"] Map.empty
    with e ->
        if e.Message = "parse error" then //fragil hack check
            raise <| SyntaxErrorException(lexbuf.StartPos)
        else
            reraise()

let emitOpCodes (il:ILGenerator) ast =
    let rec emit lenv ast =
        match ast with
        | Int32 x  -> il.Emit(OpCodes.Ldc_I4, x)
        | Double x -> il.Emit(OpCodes.Ldc_R8, x)
        | String x -> il.Emit(OpCodes.Ldstr, x)
        | Char x ->   il.Emit(OpCodes.Ldc_I4_S, byte x)
        | UMinus(x,_) -> 
            emit lenv x
            il.Emit(OpCodes.Neg)
        | NumericBinop(op,x,y,_) -> 
            emitAll lenv [x;y]
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
            
            emitAll lenv args
            
            if instance.Type.IsValueType then
                il.Emit(OpCodes.Call, meth)
            else
                il.Emit(OpCodes.Callvirt, meth)
        | Sequential(x,y,_) ->
            emit lenv x
            if x.Type <> typeof<System.Void> then il.Emit(OpCodes.Pop)
            emit lenv y
        | Ctor(ctor, args, _) -> 
            emitAll lenv args
            il.Emit(OpCodes.Newobj, ctor)
    and emitAll lenv exps =
        for arg in exps do 
            emit lenv arg

    emit Map.empty ast |> ignore

let dmFromAst (ast:texp) =
    let dm = System.Reflection.Emit.DynamicMethod("NewLang", ast.Type, null)
    let il = dm.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Ret)
    dm

let dmFromString = parseFromString>>dmFromAst

let eval<'a> code : 'a = (dmFromString code).Invoke(null,null) |> unbox

open System.Reflection

///ast -> assemblyName -> unit
let compileFromAst ast asmName =
    let asmFileName = asmName + ".exe"
    let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndSave)
    let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".netmodule", asmFileName, false) //need to specify asmFileName here!
    let tyBuilder = modBuilder.DefineType(asmName + ".Application", TypeAttributes.Public)
    let methBuilder = tyBuilder.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Static, typeof<System.Void>, null)
    
    let il = methBuilder.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Ret)

    tyBuilder.CreateType() |> ignore
    asmBuilder.SetEntryPoint(methBuilder, PEFileKinds.ConsoleApplication)
    asmBuilder.Save(asmFileName)

///code -> assemblyName -> unit
let compileFromString = parseFromString>>compileFromAst

///fileNames -> assemblyName -> unit
let compileFromFiles fileNames =
    fileNames
    |> Seq.map System.IO.File.ReadAllText
    |> String.concat System.Environment.NewLine
    |> compileFromString