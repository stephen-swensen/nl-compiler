module Swensen.NL.Compilation

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

type EL = ErrorLogger

let emitOpCodes (il:ILGenerator) ast =
    let rec emitWith loopLabel lenv ast =
        let emit = emitWith loopLabel lenv
        let emitAll exps =
            for arg in exps do 
                emit arg

        match ast with
        | Int32 x  -> il.Emit(OpCodes.Ldc_I4, x)
        | Double x -> il.Emit(OpCodes.Ldc_R8, x)
        | String x -> il.Emit(OpCodes.Ldstr, x)
        | Char x   -> il.Emit(OpCodes.Ldc_I4_S, byte x)
        | Bool x   -> il.Emit(if x then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        | Null ty  -> il.Emit(OpCodes.Ldnull)
        | UMinus(x,_) -> 
            emit x
            il.Emit(OpCodes.Neg)
        | NumericBinop(op,x,y,_) -> 
            emitAll [x;y]
            match op with
            | Plus  -> il.Emit(OpCodes.Add)
            | Minus -> il.Emit(OpCodes.Sub)
            | Times -> il.Emit(OpCodes.Mul)
            | Div   -> il.Emit(OpCodes.Div)
        | ComparisonBinop(op,x,y) -> 
            emitAll [x;y]
            match op with
            | Eq -> il.Emit(OpCodes.Ceq)
            | Lt -> il.Emit(OpCodes.Clt)
            | Gt -> il.Emit(OpCodes.Cgt)
        | texp.Let(name, assign, body,_) ->
            let local = il.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
            emit assign
            il.Emit(OpCodes.Stloc, local)
            emitWith loopLabel (Map.add name local lenv) body
        | Var(name, _) ->
            let local = lenv |> Map.find name
            il.Emit(OpCodes.Ldloc, local)
        | VarSet(name, x) ->
            let local = lenv |> Map.find name
            emit x
            il.Emit(OpCodes.Stloc, local)
        | Coerce(x,ty) ->
            emit x
            if ty = typeof<float> then
                il.Emit(OpCodes.Conv_R8)
            elif ty = typeof<int> then
                il.Emit(OpCodes.Conv_I4)
            else
                failwithf "unsupported coersion: %A" ty //shouldn't be possible since already ty checked
        | Cast(x,ty) -> //precondition: x.Type <> ty
            emit x
            if x.Type.IsValueType then
                il.Emit(OpCodes.Box,x.Type)
                if ty <> typeof<obj> then //box value type to an interface
                    il.Emit(OpCodes.Castclass, ty)
            elif ty.IsValueType then
                il.Emit(OpCodes.Unbox_Any, ty)
            else
                il.Emit(OpCodes.Castclass, ty)
        | StaticCall(meth,args,_) ->
            args |> List.iter (emit)
            il.Emit(OpCodes.Call, meth)
        | InstanceCall(instance,meth,args,_) ->
            emit instance
            if instance.Type.IsValueType then
                let loc = il.DeclareLocal(instance.Type)
                il.Emit(OpCodes.Stloc, loc)
                il.Emit(OpCodes.Ldloca, loc)
            
            emitAll args
            
            if instance.Type.IsValueType then
                il.Emit(OpCodes.Call, meth)
            else
                il.Emit(OpCodes.Callvirt, meth)
        | Sequential(x,y,_) ->
            emit x
            if x.Type <> typeof<System.Void> then il.Emit(OpCodes.Pop)
            emit y
        | Ctor(ctor, args, _) -> 
            emitAll args
            il.Emit(OpCodes.Newobj, ctor)
        | Typeof(ty) ->
            //learned through C# ildasm
            il.Emit(OpCodes.Ldtoken, ty)
            il.Emit(OpCodes.Call, typeof<Type>.GetMethod("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|]))
        | Default(ty) ->
            //start with primitive optimizations
            if ty = typeof<int32> then
                emit <| texp.Int32(Unchecked.defaultof<int32>)
            elif ty = typeof<double> then
                emit <| texp.Double(Unchecked.defaultof<double>)
            elif ty = typeof<bool> then
                emit <| texp.Bool(Unchecked.defaultof<bool>)
            elif ty = typeof<char> then
                emit <| texp.Char(Unchecked.defaultof<char>)
            else //http://source.db4o.com/db4o/trunk/db4o.net/Libs/compact-3.5/System.Linq.Expressions/System.Linq.Expressions/EmitContext.cs
                let loc = il.DeclareLocal(ty)
                il.Emit(OpCodes.Ldloca, loc)
                il.Emit(OpCodes.Initobj, ty)
                il.Emit(OpCodes.Ldloc, loc)
        | LogicalNot(x) ->
            emit x
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)
        | IfThen(x,y) ->
            let endIfLabel = il.DefineLabel()
            emit x
            il.Emit(OpCodes.Brfalse_S, endIfLabel)
            emit y
            il.MarkLabel(endIfLabel)
        | IfThenElse(x,y,z,_) ->
            let endIfLabel = il.DefineLabel()
            let beginElseLabel = il.DefineLabel()
            emit x
            il.Emit(OpCodes.Brfalse_S, beginElseLabel)
            emit y
            il.Emit(OpCodes.Br, endIfLabel)
            il.MarkLabel(beginElseLabel)
            emit z
            il.MarkLabel(endIfLabel)
        | Nop -> ()
        | WhileLoop(condition, body) ->
            let beginConditionLabel = il.DefineLabel()
            let endBodyLabel = il.DefineLabel()
            il.MarkLabel(beginConditionLabel)
            emit condition
            il.Emit(OpCodes.Brfalse_S, endBodyLabel)
            emitWith (Some(beginConditionLabel, endBodyLabel)) lenv body
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
                failwith "invalid break"
//        | Xor(x,y) ->
//            emitAll [x;y]
//            il.Emit(OpCodes.Xor)
        | Error _ ->
            failwith "Should not be emitting opcodes for an ast with errors"

    emitWith None Map.empty ast |> ignore

///parse from the lexbuf with the given semantic environment
let parseWith env lexbuf =
    try 
        let x = Parser.start Lexer.tokenize lexbuf 
        let x =
            match x with
            | rnl.Exp(x) -> x
            | _ -> failwithf "%A" x
        
        x |> SemanticAnalysis.tycheckWith env
    with
    | CompilerInterruptException ->
        texp.Error(typeof<Void>)
    //fslex/yacc do not use specific exception types
    | e when e.Message = "parse error" || e.Message = "unrecognized input" ->
        EL.ActiveLogger.Log
            (CompilerError(PositionRange(lexbuf.StartPos,lexbuf.EndPos), ErrorType.Syntactic, ErrorLevel.Error, -1, e.Message, null)) //todo: we want the real StackTrace
        texp.Error(typeof<Void>)
    | e ->
        EL.ActiveLogger.Log
            (CompilerError(PositionRange(lexbuf.StartPos,lexbuf.EndPos), ErrorType.Internal, ErrorLevel.Error, -1, e.ToString(), null))  //todo: we want the real StackTrace
        texp.Error(typeof<Void>)

///parse from the string with the given semantic environment
let parseFromStringWith env code =
    let lexbuf = 
        let lexbuf = LexBuffer<char>.FromString(code)
        lexbuf.EndPos <- { pos_bol = 0
                           pos_fname=""
                           pos_cnum=1
                           pos_lnum=1 }
        lexbuf

    parseWith env lexbuf

///parseFromString with the "default" environment
let parseFromString = parseFromStringWith SemanticEnvironment.Default

///Create a dynamic method from a typed expression using the default environment
let dmFromAst (ast:texp) =
    let dm = DynamicMethod("Eval", ast.Type, null)
    let il = dm.GetILGenerator()
    emitOpCodes il ast
    il.Emit(OpCodes.Ret)
    dm

//should have "tryEval" which doesn't throw?

///Evaluate an NL code string using the default environment.
///If one or more compiler errors occur, then an EvaluationException is throw which contains the list of errors. Warnings are ignored.
let eval<'a> code : 'a = 
    EL.InstallDefaultLogger() //may want to switch back to previous logger when exiting eval
    let ast = parseFromString code
    match EL.ActiveLogger.ErrorCount with
    | 0 ->
        let dm = (dmFromAst ast)
        dm.Invoke(null,null) |> unbox
    | _ ->
        raise (EvaluationException(EL.ActiveLogger.Errors |> Seq.toArray))

//TODO: the following methods are used for nlc.exe and nli.exe. in the future we will have 
//specialized methodss for the Compiler service, vs. nlc.exe, vs. nli.exe with APPRORPIATE ERRORLOGGERS installed.

//todo: currently this function compilies an expression to a single method
//      set as the entry point of an assembly, obviously this needs to evolve.
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

///Compile the given source code string into an assembly
///code -> assemblyName -> unit
let compileFromString = parseFromString>>compileFromAst

///Compile all the given source code files into a single assembly
///fileNames -> assemblyName -> unit
let compileFromFiles fileNames =
    fileNames
    |> Seq.map System.IO.File.ReadAllText
    |> String.concat System.Environment.NewLine
    |> compileFromString

//type NliState =
//    {
//        Assemblies: Assembly list
//        Namespaces: string list
//        Variables: (string,(obj*Type)) Map
//    }
//    with
//        static member Empty = {Assemblies = []; Namespaces=[]; Variables=Map.empty}
//        static member Default =
//            let se = Semant.SemanticEnvironment.Default
//            { NliState.Empty with Assemblies=se.Assemblies; Namespaces=se.Namespaces }
//
/////The NL interactive
//type Nli() = 
//    ///head of list is most recent, this should not be externally mutable
//    let mutable historicalState: NliState list = []
//    ///the current state, this can be externally mutable if so desired
//    let mutable currentState: NliState = NliState.Default
//
//    ///evaluate the code string in the Nli environment
//    let eval code =
//        //WE NEED THE RAW AST FIRST
//        let lexbuf = 
//            let lexbuf = LexBuffer<char>.FromString(code)
//            lexbuf.EndPos <- { pos_bol = 0
//                               pos_fname=""
//                               pos_cnum=1
//                               pos_lnum=1 }
//            lexbuf
//
//        let ast = parseFromStringWith { Semant.SemanticEnvironment.Empty with Assemblies=currentState.Assemblies; Namespaces=currentState.Namespaces; Variables=currentState.Variables |> Map.map (fun _ (_,ty)->ty) } code
//        let dm = System.Reflection.Emit.DynamicMethod("Eval", ast.Type, null)
//            let il = dm.GetILGenerator()
//            emitOpCodes il (Return(ast, ast.Type))
//            dm
//
//        for a in currentState.Assemblies do