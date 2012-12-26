namespace Swensen.NL

open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic

///A wrapper around the standard ILGenerator, which adds smart features
///like correct statically type Emit overloads and opcode trace.
type SmartILGenerator(ilgen:ILGenerator) =
    let opcodes = List<OpCode * (obj option)>()
    let debug = 
        #if DEBUG 
            true
        #else
            false
        #endif

    ///Access to the underlying ILGenerator. n.b. that if Emit is used directly then the OpCodes trace will have holes.
    member this.ILGenerator = ilgen
    ///A trace of the opcodes (with args) which have been emitted (in order from first to last). This is only enabled
    ///if the DEBUG symbol is defined during compilation. n.b. that using if ILGenerator.Emit is used directly, then 
    ///this trace wll have holes.
    member this.OpCodes = opcodes |> Seq.readonly

    member this.Emit(oc:OpCode) =
        if debug then opcodes.Add(oc,None)
        ilgen.Emit(oc)
    member this.Emit(oc:OpCode, x:FieldInfo) =
        if debug then opcodes.Add(oc,Some(x:>obj))
        ilgen.Emit(oc,x)
    member this.Emit(oc:OpCode, x:LocalBuilder) =
        if debug then opcodes.Add(oc,Some(x:>obj))
        ilgen.Emit(oc,x)
    member this.Emit(oc:OpCode, x:Type) =
        if debug then opcodes.Add(oc,Some(x:>obj))
        ilgen.Emit(oc,x)
    member this.Emit(oc:OpCode, x:ConstructorInfo) =
        if debug then opcodes.Add(oc,Some(x:>obj))
        ilgen.Emit(oc,x)
    member this.Emit(oc:OpCode, x:MethodInfo) =
        if debug then opcodes.Add(oc,Some(x:>obj))
        ilgen.Emit(oc,x)
    member this.Emit(oc:OpCode, x:Label) =
        if debug then opcodes.Add(oc,Some(x:>obj))
        ilgen.Emit(oc,x)

    member this.Ldsfld(fi:FieldInfo) =
        this.Emit(OpCodes.Ldsfld, fi)
    member this.Ldflda(fi:FieldInfo) =
        this.Emit(OpCodes.Ldflda, fi)
    member this.Ldfld(fi:FieldInfo) =
        this.Emit(OpCodes.Ldfld, fi)
    member this.Ldsflda(fi:FieldInfo) = 
        this.Emit(OpCodes.Ldsflda, fi)
    member this.Ldloca(local: LocalBuilder) =
        this.Emit(OpCodes.Ldloca, local)
    member this.Ldloc(local :LocalBuilder) =
        this.Emit(OpCodes.Ldloc, local)
    member this.Ldnull() =
        this.Emit(OpCodes.Ldnull)
    member this.Stfld(fi:FieldInfo) =
        this.Emit(OpCodes.Stfld, fi)
    member this.Stsfld(fi:FieldInfo) =
        this.Emit(OpCodes.Stsfld, fi)
    member this.Stloc(local: LocalBuilder) =
        this.Emit(OpCodes.Stloc, local)
    member this.Initobj(ty:Type) =
        this.Emit(OpCodes.Initobj, ty)
    member this.Neg() =
        this.Emit(OpCodes.Neg)
    member this.Add() =
        this.Emit(OpCodes.Add)
    member this.Sub() =
        this.Emit(OpCodes.Sub)
    member this.Mul() =
        this.Emit(OpCodes.Mul)
    member this.Div() =
        this.Emit(OpCodes.Div)
    member this.Ceq() =
        this.Emit(OpCodes.Ceq)
    member this.Cgt() =
        this.Emit(OpCodes.Cgt)
    member this.Clt() =
        this.Emit(OpCodes.Clt)
    member this.Pop() =
        this.Emit(OpCodes.Pop)
    member this.Castclass(ty:Type) =
        this.Emit(OpCodes.Castclass, ty)
    member this.Unbox_Any(ty:Type) =
        this.Emit(OpCodes.Unbox_Any, ty)
    member this.Box(ty:Type) =
        this.Emit(OpCodes.Box, ty)
    member this.Ldtoken(ty:Type) =
        this.Emit(OpCodes.Ldtoken, ty)
    member this.Newobj(ctor:ConstructorInfo) =
        this.Emit(OpCodes.Newobj, ctor)
    member this.Call(meth:MethodInfo) =
        this.Emit(OpCodes.Call, meth)
    member this.Callvirt(meth:MethodInfo) =
        this.Emit(OpCodes.Callvirt, meth)
    member this.Brfalse_S(label:Label) =
        this.Emit(OpCodes.Brfalse_S, label)
    member this.Br(label:Label) =
        this.Emit(OpCodes.Br, label)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SmartILGenerator =
    let fromILGenerator (il:ILGenerator) =
        SmartILGenerator(il)