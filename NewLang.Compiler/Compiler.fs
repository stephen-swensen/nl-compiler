module Swensen.NewLang.Compiler

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
            false
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
    let rec emit loopLabel lenv ast =
        let fastEmit = emit loopLabel lenv
        let fastEmitAll exps =
            for arg in exps do 
                fastEmit arg

        match ast with
        | Int32 x  -> il.Emit(OpCodes.Ldc_I4, x)
        | Double x -> il.Emit(OpCodes.Ldc_R8, x)
        | String x -> il.Emit(OpCodes.Ldstr, x)
        | Char x   -> il.Emit(OpCodes.Ldc_I4_S, byte x)
        | Bool x   -> il.Emit(if x then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        | Null ty  -> il.Emit(OpCodes.Ldnull)
        | UMinus(x,_) -> 
            fastEmit x
            il.Emit(OpCodes.Neg)
        | NumericBinop(op,x,y,_) -> 
            fastEmitAll [x;y]
            match op with
            | Plus  -> il.Emit(OpCodes.Add)
            | Minus -> il.Emit(OpCodes.Sub)
            | Times -> il.Emit(OpCodes.Mul)
            | Div   -> il.Emit(OpCodes.Div)
        | ComparisonBinop(op,x,y) -> 
            fastEmitAll [x;y]
            match op with
            | Eq -> il.Emit(OpCodes.Ceq)
            | Lt -> il.Emit(OpCodes.Clt)
            | Gt -> il.Emit(OpCodes.Cgt)
        | Let(name, assign, body,_) ->
            let local = il.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
            fastEmit assign
            il.Emit(OpCodes.Stloc, local)
            emit loopLabel (Map.add name local lenv) body
        | Var(name, _) ->
            let local = lenv |> Map.find name
            il.Emit(OpCodes.Ldloc, local)
        | VarSet(name, x) ->
            let local = lenv |> Map.find name
            fastEmit x
            il.Emit(OpCodes.Stloc, local)
        | Coerce(x,ty) ->
            fastEmit x
            if ty = typeof<float> then
                il.Emit(OpCodes.Conv_R8)
            elif ty = typeof<int> then
                il.Emit(OpCodes.Conv_I4)
            else
                failwithf "unsupported coersion: %A" ty //shouldn't be possible since already ty checked
        | Cast(x,ty) -> //precondition: x.Type <> ty
            fastEmit x
            if x.Type.IsValueType then
                il.Emit(OpCodes.Box,x.Type)
                if ty <> typeof<obj> then //box value type to an interface
                    il.Emit(OpCodes.Castclass, ty)
            elif ty.IsValueType then
                il.Emit(OpCodes.Unbox_Any, ty)
            else
                il.Emit(OpCodes.Castclass, ty)
        | StaticCall(meth,args,_) ->
            args |> List.iter (fastEmit)
            il.Emit(OpCodes.Call, meth)
        | InstanceCall(instance,meth,args,_) ->
            fastEmit instance
            if instance.Type.IsValueType then
                let loc = il.DeclareLocal(instance.Type)
                il.Emit(OpCodes.Stloc, loc)
                il.Emit(OpCodes.Ldloca, loc)
            
            fastEmitAll args
            
            if instance.Type.IsValueType then
                il.Emit(OpCodes.Call, meth)
            else
                il.Emit(OpCodes.Callvirt, meth)
        | Sequential(x,y,_) ->
            fastEmit x
            if x.Type <> typeof<System.Void> then il.Emit(OpCodes.Pop)
            fastEmit y
        | Ctor(ctor, args, _) -> 
            fastEmitAll args
            il.Emit(OpCodes.Newobj, ctor)
        | Typeof(ty) ->
            //learned through C# ildasm
            il.Emit(OpCodes.Ldtoken, ty)
            il.Emit(OpCodes.Call, typeof<Type>.GetMethod("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|]))
        | Default(ty) ->
            //start with primitive optimizations
            if ty = typeof<int32> then
                fastEmit <| texp.Int32(Unchecked.defaultof<int32>)
            elif ty = typeof<double> then
                fastEmit <| texp.Double(Unchecked.defaultof<double>)
            elif ty = typeof<bool> then
                fastEmit <| texp.Bool(Unchecked.defaultof<bool>)
            elif ty = typeof<char> then
                fastEmit <| texp.Char(Unchecked.defaultof<char>)
            else //http://source.db4o.com/db4o/trunk/db4o.net/Libs/compact-3.5/System.Linq.Expressions/System.Linq.Expressions/EmitContext.cs
                let loc = il.DeclareLocal(ty)
                il.Emit(OpCodes.Ldloca, loc)
                il.Emit(OpCodes.Initobj, ty)
                il.Emit(OpCodes.Ldloc, loc)
        | Not(x,_) ->
            fastEmit x
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)
        | IfThen(x,y) ->
            let endIfLabel = il.DefineLabel()
            fastEmit x
            il.Emit(OpCodes.Brfalse_S, endIfLabel)
            fastEmit y
            il.MarkLabel(endIfLabel)
        | IfThenElse(x,y,z,_) ->
            let endIfLabel = il.DefineLabel()
            let beginElseLabel = il.DefineLabel()
            fastEmit x
            il.Emit(OpCodes.Brfalse_S, beginElseLabel)
            fastEmit y
            il.Emit(OpCodes.Br, endIfLabel)
            il.MarkLabel(beginElseLabel)
            fastEmit z
            il.MarkLabel(endIfLabel)
        | Nop -> ()
        | WhileLoop(condition, body) ->
            let beginConditionLabel = il.DefineLabel()
            let endBodyLabel = il.DefineLabel()
            il.MarkLabel(beginConditionLabel)
            fastEmit condition
            il.Emit(OpCodes.Brfalse_S, endBodyLabel)
            emit (Some(beginConditionLabel, endBodyLabel)) lenv body
            if body.Type <> typeof<Void> then
                il.Emit(OpCodes.Pop)
            il.Emit(OpCodes.Br, beginConditionLabel)
            il.MarkLabel(endBodyLabel)
        | Continue ->
            match loopLabel with
            | Some(beginConditionLabel,_) ->
                il.Emit(OpCodes.Br, beginConditionLabel)
            | None ->
                failwith "invalid continue"
        | Break ->
            match loopLabel with
            | Some(_, endBodyLabel) ->
                il.Emit(OpCodes.Br, endBodyLabel)
            | None ->
                failwith "break"               

    emit None Map.empty ast |> ignore

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