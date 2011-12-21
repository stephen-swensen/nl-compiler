module Swensen.NL.Emission
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

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
        let setLocalVar (local:LocalBuilder) assign =
            if isDefaultOfValueType assign then
                il.Emit(OpCodes.Ldloca, local)
                il.Emit(OpCodes.Initobj, assign.Type)
            else
                emit assign
                il.Emit(OpCodes.Stloc, local)

        match ilExpr with
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
            | ILNumericBinop.Plus  -> il.Emit(OpCodes.Add)
            | ILNumericBinop.Minus -> il.Emit(OpCodes.Sub)
            | ILNumericBinop.Times -> il.Emit(OpCodes.Mul)
            | ILNumericBinop.Div   -> il.Emit(OpCodes.Div)
        | ComparisonBinop(op,x,y) -> 
            emitAll [x;y]
            match op with
            | Eq -> il.Emit(OpCodes.Ceq)
            | Lt -> il.Emit(OpCodes.Clt)
            | Gt -> il.Emit(OpCodes.Cgt)
        | ILExpr.Let(name, assign, body,_) ->
            let local = il.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
            setLocalVar local assign
            emitWith loopLabel (Map.add name local lenv) body
        | VarGet(name, _) ->
            let local = lenv |> Map.find name
            il.Emit(OpCodes.Ldloc, local)
        | VarSet(name, assign) ->
            let local = lenv |> Map.find name
            setLocalVar local assign
        | Coerce(x,ty) ->
            emit x
            let convLookup = 
                [
                    //typeof<>, OpCodes.Conv_I
                    typeof<int8>, OpCodes.Conv_I1
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
                ] |> dict //can't use Map because Type does not support "comparison" constraints

            match convLookup.TryGetValue ty with
            | true, oc ->
                il.Emit(oc)
            | _ ->
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
        | StaticCall(meth,args) ->
            args |> List.iter (emit)
            il.Emit(OpCodes.Call, meth)
        | InstanceCall(instance,meth,args) ->
            let isValueAddress = emitValueAddressIfApplicable loopLabel lenv instance
            if not isValueAddress && instance.Type.IsValueType then
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
            if not ty.IsValueType then
                //although we could technically use initobj to emit a null reference of a non-value type, it is really only
                //designed for value types and therefore we treat it as null here and don't consider it an optimization
                //(unless we see some good reason in the optimization phase).
                emit <| Null(ty)
            else
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
        | StaticFieldSet(fi, assign) ->            
            if isDefaultOfValueType assign then
                il.Emit(OpCodes.Ldsflda, fi)
                il.Emit(OpCodes.Initobj, assign.Type)
            else
                emit assign
                il.Emit(OpCodes.Stsfld, fi)
        | StaticFieldGet(fi) ->
            il.Emit(OpCodes.Ldsfld, fi)
        | InstanceFieldSet(instance, fi, assign) ->
            emitValueAddressIfApplicable loopLabel lenv instance |> ignore
            if isDefaultOfValueType assign then
                il.Emit(OpCodes.Ldflda, fi)
                il.Emit(OpCodes.Initobj, assign.Type)
            else
                emit assign
                il.Emit(OpCodes.Stfld, fi)
        | InstanceFieldGet(instance, fi) ->
            emitValueAddressIfApplicable loopLabel lenv instance |> ignore
            il.Emit(OpCodes.Ldfld, fi)
        | ILExpr.Error _ ->
            failwith "Should not be emitting opcodes for an ilExpr with errors"

    ///e.g. Given: SomeVar.field <- 3, we need the (ADDRESS OF)SomeVar.(ADDRESS OF)field <- 3 when SomeVar and field are value types (if they are object types,
    ///we are already getting the address by nature.
    and emitValueAddressIfApplicable loopLabel lenv expr : bool =
        match expr with
        | InstanceFieldGet(instance, fi) when fi.FieldType.IsValueType ->
            emitValueAddressIfApplicable loopLabel lenv instance |> ignore
            il.Emit(OpCodes.Ldflda, fi)
            true
        | StaticFieldGet(fi) when fi.FieldType.IsValueType ->
            il.Emit(OpCodes.Ldsflda, fi)
            true
        | VarGet(name, ty) when ty.IsValueType ->
            let local = lenv |> Map.find name
            il.Emit(OpCodes.Ldloca, local)
            true
        | _ -> 
            emitWith loopLabel lenv expr
            false
    //and emitValueInitForSetIfApplicable loopLabel

    emitWith None Map.empty ilExpr