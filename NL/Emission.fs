module Swensen.NL.Emission
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

///Emit opcodes for the given ILExpr on the given ILGenerator. An exception will be raised if the expression tree contains any errors.
let emit optimize (il:SmartILGenerator) ilExpr =
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
        | Byte x -> il.Ldc_I4_S(x)
        | SByte x -> il.Ldc_I4_S(x)
        | Int16 x -> il.Ldc_I4(int32 x) //not sure why needs to be conv to int32 first (has a int16 overload but results in invalid program)
        | Int32 x -> il.Ldc_I4(x)
        | Int64 x -> il.Ldc_I8(x)
        | UInt16 x -> il.Ldc_I4(int32 x) //not sure why needs to be conv to int32 first (has a int16 overload but results in invalid program)
        | UInt32 x -> il.Ldc_I4(int32 x)             
        | UInt64 x -> il.Ldc_I8(int64 x)
        | Single x -> il.Ldc_R4(x)
        | Double x -> il.Ldc_R8(x)
        | String x -> il.Ldstr(x)
        | Char x -> il.Ldc_I4_S(int8 x) //should be ushort (uint16) not byte (uint8)?
        | Bool x -> il.Emit(if x then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        | Null ty -> il.Ldnull()
        | UMinus(cked, x, _) -> 
            emit x
            il.Neg()
        | NumericBinop(cked, op,x,y,_) -> 
            emitAll [x;y]
            match op with
            | ILNumericBinop.Plus -> il.Add()
            | ILNumericBinop.Minus -> il.Sub()
            | ILNumericBinop.Times -> il.Mul()
            | ILNumericBinop.Div -> il.Div()
        | ComparisonBinop(op,x,y) -> 
            emitAll [x;y]
            match op with
            | Eq -> il.Ceq()
            | Lt -> il.Clt()
            | Gt -> il.Cgt()
        | ILExpr.Let(name, assign, body,_) ->
            let local = il.ILGenerator.DeclareLocal(assign.Type) //can't use local.SetLocalSymInfo(id) in dynamic assemblies / methods
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
                let loc = il.ILGenerator.DeclareLocal(instance.Type)
                il.Stloc(loc)
                il.Ldloca(loc)
            
            emitAll args
            
            if instance.Type.IsValueType then
                il.Call(meth)
            else
                il.Callvirt(meth)
        | Sequential(x,y,_) ->
            emit x
            if not <| isVoidOrEscapeTy x.Type then il.Pop()
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
                let loc = il.ILGenerator.DeclareLocal(ty)
                il.Ldloca(loc)
                il.Initobj(ty)
                il.Ldloc(loc)
        | LogicalNot(x) ->
            emit x
            il.Emit(OpCodes.Ldc_I4_0)
            il.Ceq()
        | IfThen(x,y) ->
            let endIfLabel = il.ILGenerator.DefineLabel()
            emit x
            il.Brfalse_S(endIfLabel)
            emit y
            il.ILGenerator.MarkLabel(endIfLabel)
        | IfThenElse(x,y,z,_) ->
            let endIfLabel = il.ILGenerator.DefineLabel()
            let beginElseLabel = il.ILGenerator.DefineLabel()
            emit x
            il.Brfalse_S(beginElseLabel)
            emit y
            il.Br(endIfLabel)
            il.ILGenerator.MarkLabel(beginElseLabel)
            emit z
            il.ILGenerator.MarkLabel(endIfLabel)
        | Nop -> if optimize then () else il.Nop()
        | WhileLoop(condition, body) ->
            let beginConditionLabel = il.ILGenerator.DefineLabel()
            let endBodyLabel = il.ILGenerator.DefineLabel()
            il.ILGenerator.MarkLabel(beginConditionLabel)
            emit condition
            il.Brfalse_S(endBodyLabel)
            emitWith (Some(beginConditionLabel, endBodyLabel)) lenv body
            if not <| isVoidOrEscapeTy body.Type then
                il.Pop()
            il.Br(beginConditionLabel)
            il.ILGenerator.MarkLabel(endBodyLabel)
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
        | ILExpr.Throw(x) ->
            emit x
            il.Emit(OpCodes.Throw)
        | ILExpr.Rethrow ->
            il.Emit(OpCodes.Rethrow)
        | ILExpr.TryCatchFinally(tx, catchList, fx, ty) ->
            //for non-void expressions, a local var for the return value of the try or catch blocks
            let retval = if isVoidOrEscapeTy tx.Type then None else Some(il.ILGenerator.DeclareLocal(ty))

            il.ILGenerator.BeginExceptionBlock() |> ignore
            emit tx
            retval |> Option.iter (fun retval -> il.Stloc(retval))
            for catch in catchList do                   
                match catch with
                | (filterTy, Some(filterName), cx) ->
                    il.ILGenerator.BeginCatchBlock(filterTy)
                    let local = il.ILGenerator.DeclareLocal(filterTy)
                    il.Stloc(local)
                    emitWith loopLabel (Map.add filterName local lenv) cx
                | (filterTy, None, cx) ->
                    il.ILGenerator.BeginCatchBlock(filterTy)
                    il.Pop()
                    emit cx
                retval |> Option.iter (fun retval -> il.Stloc(retval))
            match fx with
            | Some(fx) ->
                il.ILGenerator.BeginFinallyBlock()
                emit fx
            | None -> ()
            
            il.ILGenerator.EndExceptionBlock()
            retval |> Option.iter (fun retval -> il.Ldloc(retval)) //load the return value of the try or catch(es), if any
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