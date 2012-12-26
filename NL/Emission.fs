module Swensen.NL.Emission
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

type ILGenerator with
    member il.Ldsfld(fi:FieldInfo) =
        il.Emit(OpCodes.Ldsfld, fi)
    member il.Ldflda(fi:FieldInfo) =
        il.Emit(OpCodes.Ldflda, fi)
    member il.Ldfld(fi:FieldInfo) =
        il.Emit(OpCodes.Ldfld, fi)
    member il.Ldsflda(fi:FieldInfo) = 
        il.Emit(OpCodes.Ldsflda, fi)
    member il.Ldloca(local: LocalBuilder) =
        il.Emit(OpCodes.Ldloca, local)
    member il.Ldloc(local :LocalBuilder) =
        il.Emit(OpCodes.Ldloc, local)
    member il.Ldnull() =
        il.Emit(OpCodes.Ldnull)
    member il.Stfld(fi:FieldInfo) =
        il.Emit(OpCodes.Stfld, fi)
    member il.Stsfld(fi:FieldInfo) =
        il.Emit(OpCodes.Stsfld, fi)
    member il.Stloc(local: LocalBuilder) =
        il.Emit(OpCodes.Stloc, local)
    member il.Initobj(ty:Type) =
        il.Emit(OpCodes.Initobj, ty)
    member il.Neg() =
        il.Emit(OpCodes.Neg)
    member il.Add() =
        il.Emit(OpCodes.Add)
    member il.Sub() =
        il.Emit(OpCodes.Sub)
    member il.Mul() =
        il.Emit(OpCodes.Mul)
    member il.Div() =
        il.Emit(OpCodes.Div)
    member il.Ceq() =
        il.Emit(OpCodes.Ceq)
    member il.Cgt() =
        il.Emit(OpCodes.Cgt)
    member il.Clt() =
        il.Emit(OpCodes.Clt)
    member il.Pop() =
        il.Emit(OpCodes.Pop)
    member il.Castclass(ty:Type) =
        il.Emit(OpCodes.Castclass, ty)
    member il.Unbox_Any(ty:Type) =
        il.Emit(OpCodes.Unbox_Any, ty)
    member il.Box(ty:Type) =
        il.Emit(OpCodes.Box, ty)
    member il.Ldtoken(ty:Type) =
        il.Emit(OpCodes.Ldtoken, ty)
    member il.Newobj(ctor:ConstructorInfo) =
        il.Emit(OpCodes.Newobj, ctor)
    member il.Call(meth:MethodInfo) =
        il.Emit(OpCodes.Call, meth)
    member il.Callvirt(meth:MethodInfo) =
        il.Emit(OpCodes.Callvirt, meth)
    member il.Brfalse_S(label:Label) =
        il.Emit(OpCodes.Brfalse_S, label)
    member il.Br(label:Label) =
        il.Emit(OpCodes.Br, label)

///Emit opcodes for the given ILExpr on the given ILGenerator. An exception will be raised if the expression tree contains any errors.
let emit (il:ILGenerator) ilExpr =
    let isDefaultOfValueType = function
        | Default(ty) when ty.IsValueType ->
            true
        | _ ->
            false

    //the loop label options is some if we are inside a while loop and gives a point to jump back when encounter continue or break
    let rec emitWith loopLabel lenv ilExpr =
        let emit = emitWith loopLabel lenv
        let emitAll exps =
            for arg in exps do 
                emit arg

        ///used by VarSet and Let expressions
        let setLocalVar (local: LocalBuilder) assign =
            if isDefaultOfValueType assign then
                il.Ldloca(local)
                il.Initobj(assign.Type)
            else
                emit assign
                il.Stloc(local)

        match ilExpr with
        | Byte x  -> il.Emit(OpCodes.Ldc_I4_S, x)
        | SByte x  -> il.Emit(OpCodes.Ldc_I4_S, x)

        | Int16 x  -> il.Emit(OpCodes.Ldc_I4, int32 x) //not sure why needs to be conv to int32 first (has a int16 overload but results in invalid program)
        | Int32 x  -> il.Emit(OpCodes.Ldc_I4, x)
        | Int64 x  -> il.Emit(OpCodes.Ldc_I8, x)

        | UInt16 x  -> il.Emit(OpCodes.Ldc_I4, int32 x) //not sure why needs to be conv to int32 first (has a int16 overload but results in invalid program)
        | UInt32 x  -> il.Emit(OpCodes.Ldc_I4, int32 x)
        | UInt64 x  -> il.Emit(OpCodes.Ldc_I8, int64 x)
        
        | Single x -> il.Emit(OpCodes.Ldc_R4, x)
        | Double x -> il.Emit(OpCodes.Ldc_R8, x)

        | String x -> il.Emit(OpCodes.Ldstr, x)
        | Char x   -> il.Emit(OpCodes.Ldc_I4_S, int8 x) //should be ushort (uint16) not byte (uint8)?
        
        | Bool x   -> il.Emit(if x then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)

        | Null ty  -> il.Ldnull()
        | UMinus(cked, x, _) -> 
            emit x
            il.Neg()
        | NumericBinop(cked, op,x,y,_) -> 
            emitAll [x;y]
            match op with
            | ILNumericBinop.Plus  -> il.Add()
            | ILNumericBinop.Minus -> il.Sub()
            | ILNumericBinop.Times -> il.Mul()
            | ILNumericBinop.Div   -> il.Div()
        | ComparisonBinop(op,x,y) -> 
            emitAll [x;y]
            match op with
            | Eq -> il.Ceq()
            | Lt -> il.Clt()
            | Gt -> il.Cgt()
        | ILExpr.Let(name, assign, body,_) ->
            let local = il.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
            setLocalVar local assign
            emitWith loopLabel (Map.add name local lenv) body
        | VarGet(name, _) ->
            let local = lenv |> Map.find name
            il.Ldloc(local)
        | VarSet(name, assign) ->
            let local = lenv |> Map.find name
            setLocalVar local assign
        | Coerce(cked, x,ty) ->
            emit x
            let convLookup = 
                [
                    //typeof<>, OpCodes.Conv_I
                    typeof<int8>, 
//                        if cked then
//                            if x.Type.isun 
//                            OpCodes.Conv_R_Un
//                        else
                            OpCodes.Conv_I1
                    typeof<int16>, OpCodes.Conv_I2
                    typeof<int32>, OpCodes.Conv_I4
                    typeof<int64>, OpCodes.Conv_I8
                    typeof<single>, OpCodes.Conv_R4
                    typeof<double>, OpCodes.Conv_R8
                    //typeof<>, OpCodes.Conv_U
                    typeof<uint8>, OpCodes.Conv_U1
                    typeof<uint16>, OpCodes.Conv_U2
                    typeof<uint32>, OpCodes.Conv_U4
                    typeof<uint64>, OpCodes.Conv_U8
                    typeof<char>, OpCodes.Conv_I4
                ] |> dict //can't use Map because Type does not support "comparison" constraints

            match convLookup.TryGetValue ty with
            | true, oc ->
                il.Emit(oc)
            | _ ->
                failwithf "unsupported coersion from '%s' to '%s'" x.Type.Name ty.Name //shouldn't be possible since already ty checked
        | Cast(x,ty) -> //precondition: x.Type <> ty
            emit x
            if x.Type.IsValueType then
                il.Box(x.Type)
                if ty <> typeof<obj> then //box value type to an interface
                    il.Castclass(ty)
            elif ty.IsValueType then
                il.Unbox_Any(ty)
            else
                il.Castclass(ty)
        | StaticCall(meth,args) ->
            args |> List.iter (emit)
            il.Call(meth)
        | InstanceCall(instance,meth,args) ->
            let isValueAddress = emitValueAddressIfApplicable loopLabel lenv instance
            if not isValueAddress && instance.Type.IsValueType then
                let loc = il.DeclareLocal(instance.Type)
                il.Stloc(loc)
                il.Ldloca(loc)
            
            emitAll args
            
            if instance.Type.IsValueType then
                il.Call(meth)
            else
                il.Callvirt(meth)
        | Sequential(x,y,_) ->
            emit x
            if x.Type <> typeof<System.Void> then il.Pop()
            emit y
        | Ctor(ctor, args, _) -> 
            emitAll args
            il.Newobj(ctor)
        | Typeof(ty) ->
            //learned through C# ildasm
            il.Ldtoken(ty)
            il.Call(typeof<Type>.GetMethod("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|]))
        | Default(ty) ->
            if not ty.IsValueType then
                //although we could technically use initobj to emit a null reference of a non-value type, it is really only
                //designed for value types and therefore we treat it as null here and don't consider it an optimization
                //(unless we see some good reason in the optimization phase).
                emit <| Null(ty)
            else
                let loc = il.DeclareLocal(ty)
                il.Ldloca(loc)
                il.Initobj(ty)
                il.Ldloc(loc)
        | LogicalNot(x) ->
            emit x
            il.Emit(OpCodes.Ldc_I4_0)
            il.Ceq()
        | IfThen(x,y) ->
            let endIfLabel = il.DefineLabel()
            emit x
            il.Brfalse_S(endIfLabel)
            emit y
            il.MarkLabel(endIfLabel)
        | IfThenElse(x,y,z,_) ->
            let endIfLabel = il.DefineLabel()
            let beginElseLabel = il.DefineLabel()
            emit x
            il.Brfalse_S(beginElseLabel)
            emit y
            il.Br(endIfLabel)
            il.MarkLabel(beginElseLabel)
            emit z
            il.MarkLabel(endIfLabel)
        | Nop -> ()
        | WhileLoop(condition, body) ->
            let beginConditionLabel = il.DefineLabel()
            let endBodyLabel = il.DefineLabel()
            il.MarkLabel(beginConditionLabel)
            emit condition
            il.Brfalse_S(endBodyLabel)
            emitWith (Some(beginConditionLabel, endBodyLabel)) lenv body
            if body.Type <> typeof<Void> then
                il.Pop()
            il.Br(beginConditionLabel)
            il.MarkLabel(endBodyLabel)
        | Continue ->
            match loopLabel with
            | Some(beginConditionLabel,_) ->
                il.Br(beginConditionLabel)
            | None ->
                failwith "invalid continue"
        | Break ->
            match loopLabel with
            | Some(_, endBodyLabel) ->
                il.Br(endBodyLabel)
            | None ->
                failwith "invalid break"
        | StaticFieldSet(fi, assign) ->            
            if isDefaultOfValueType assign then
                il.Ldsflda(fi)
                il.Initobj(assign.Type)
            else
                emit assign
                il.Stsfld(fi)
        | StaticFieldGet(fi) ->
            il.Ldsfld(fi)
        | InstanceFieldSet(instance, fi, assign) ->
            emitValueAddressIfApplicable loopLabel lenv instance |> ignore
            if isDefaultOfValueType assign then
                il.Ldflda(fi)
                il.Initobj(assign.Type)
            else
                emit assign
                il.Stfld(fi)
        | InstanceFieldGet(instance, fi) ->
            emitValueAddressIfApplicable loopLabel lenv instance |> ignore
            il.Ldfld(fi)
        | ILExpr.Error _ ->
            failwith "Should not be emitting opcodes for an ilExpr with errors"

    ///e.g. Given: SomeVar.field <- 3, we need the (ADDRESS OF)SomeVar.(ADDRESS OF)field <- 3 when SomeVar and field are value types (if they are object types,
    ///we are already getting the address by nature.
    and emitValueAddressIfApplicable loopLabel lenv expr : bool =
        match expr with
        | InstanceFieldGet(instance, fi) when fi.FieldType.IsValueType ->
            emitValueAddressIfApplicable loopLabel lenv instance |> ignore
            il.Ldflda(fi)
            true
        | StaticFieldGet(fi) when fi.FieldType.IsValueType ->
            il.Ldsflda(fi)
            true
        | VarGet(name, ty) when ty.IsValueType ->
            let local = lenv |> Map.find name
            il.Ldloca(local)
            true
        | _ -> 
            emitWith loopLabel lenv expr
            false
    //and emitValueInitForSetIfApplicable loopLabel

    emitWith None Map.empty ilExpr