﻿module Swensen.NewLang.Compiler

open Microsoft.FSharp.Text.Lexing
open System
open Swensen.NewLang

open Lexer
open Parser

open  System.Reflection.Emit

let parseFromString code =
    let lexbuf = 
        let lexbuf = LexBuffer<char>.FromString(code)
        lexbuf.EndPos <- { pos_bol = 0
                           pos_fname=""
                           pos_cnum=1
                           pos_lnum=1 }
        lexbuf
    try 
        Parser.start Lexer.tokenize lexbuf
        |> Semant.tycheck 
           (["mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
             "System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
             "System.Core, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
             "System.Numerics, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"] |> List.map (fun name -> System.Reflection.Assembly.Load(name)))
            ["system"
             "system.collections"
             "system.collections.generic"
             "system.numerics"] 
            Map.empty
    with
    | e when e.Message = "parse error" || e.Message = "unrecognized input" -> //fragil hack check
        raise <| SyntaxErrorException(lexbuf.StartPos)

let emitOpCodes (il:ILGenerator) ast =
    let rec emit lenv ast =
        match ast with
        | Int32 x  -> il.Emit(OpCodes.Ldc_I4, x)
        | Double x -> il.Emit(OpCodes.Ldc_R8, x)
        | String x -> il.Emit(OpCodes.Ldstr, x)
        | Char x   -> il.Emit(OpCodes.Ldc_I4_S, byte x)
        | Bool x   -> il.Emit(if x then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        | Null ty  -> il.Emit(OpCodes.Ldnull)
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
        | ComparisonBinop(op,x,y) -> 
            emitAll lenv [x;y]
            match op with
            | Eq -> il.Emit(OpCodes.Ceq)
            | Lt -> il.Emit(OpCodes.Clt)
            | Gt -> il.Emit(OpCodes.Cgt)
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
        | Cast(x,ty) -> //precondition: x.Type <> ty
            emit lenv x
            if x.Type.IsValueType then
                il.Emit(OpCodes.Box,x.Type)
                if ty <> typeof<obj> then //box value type to an interface
                    il.Emit(OpCodes.Castclass, ty)
            elif ty.IsValueType then
                il.Emit(OpCodes.Unbox_Any, ty)
            else
                il.Emit(OpCodes.Castclass, ty)
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
        | Typeof(ty) ->
            //learned through C# ildasm
            il.Emit(OpCodes.Ldtoken, ty)
            il.Emit(OpCodes.Call, typeof<Type>.GetMethod("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|]))
        | Default(ty) ->
            //start with primitive optimizations
            if ty = typeof<int32> then
                emit lenv <| texp.Int32(Unchecked.defaultof<int32>)
            elif ty = typeof<double> then
                emit lenv <| texp.Double(Unchecked.defaultof<double>)
            elif ty = typeof<bool> then
                emit lenv <| texp.Bool(Unchecked.defaultof<bool>)
            elif ty = typeof<char> then
                emit lenv <| texp.Char(Unchecked.defaultof<char>)
            else //http://source.db4o.com/db4o/trunk/db4o.net/Libs/compact-3.5/System.Linq.Expressions/System.Linq.Expressions/EmitContext.cs
                let loc = il.DeclareLocal(ty)
                il.Emit(OpCodes.Ldloca, loc)
                il.Emit(OpCodes.Initobj, ty)
                il.Emit(OpCodes.Ldloc, loc)
        | Not(x,_) ->
            emit lenv x
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)
        | IfThen(x,y) ->
            let endIfLabel = il.DefineLabel()
            emit lenv x
            il.Emit(OpCodes.Brfalse_S, endIfLabel)
            emit lenv y
            il.MarkLabel(endIfLabel)
        | IfThenElse(x,y,z,_) ->
            let endIfLabel = il.DefineLabel()
            let beginElseLabel = il.DefineLabel()
            emit lenv x
            il.Emit(OpCodes.Brfalse_S, beginElseLabel)
            emit lenv y
            il.Emit(OpCodes.Br, endIfLabel)
            il.MarkLabel(beginElseLabel)
            emit lenv z
            il.MarkLabel(endIfLabel)

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