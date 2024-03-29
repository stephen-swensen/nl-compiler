﻿module Swensen.NL.Optimization
open Swensen.NL.Ail
open System

//we do not optimize checked operations since that might result in a compile time overflow (in the 
//future we may have all constant expressions checked at compile time, like c#)
let rec optimizeExpr exp = 
    match exp with
    | ILExpr.IfThenElse(condition, thenBranch, elseBranch, ty) -> //unreachable code elimination
        let condition, thenBranch, elseBranch = optimizeExpr condition, optimizeExpr thenBranch, optimizeExpr elseBranch
        match condition with
        | ILExpr.Bool(true) -> thenBranch
        | ILExpr.Bool(false) -> elseBranch
        | _ -> 
            //todo: HUH?! should this match be on the elseBranch?
            match thenBranch with 
            | ILExpr.Nop -> ILExpr.IfThen(condition, thenBranch)
            | _ ->
                ILExpr.IfThenElse(condition, thenBranch, elseBranch, ty)
    | ILExpr.IfThen(condition, thenBranch) -> //unreachable code elimination
        let condition, thenBranch = optimizeExpr condition, optimizeExpr thenBranch
        match condition with
        | ILExpr.Bool(true) -> thenBranch
        | ILExpr.Bool(false) -> ILExpr.Nop
        | _ -> 
            match thenBranch with
            | ILExpr.Nop -> condition
            | _ ->
                ILExpr.IfThen(condition, thenBranch)
    | ILExpr.NumericBinop(cked, op, x, y, ty) -> //numeric constants folding
        let x, y = optimizeExpr x, optimizeExpr y
        match cked, x, y with
        | false, ILExpr.SByte(xval), ILExpr.SByte(yval) ->
            ILExpr.SByte(op.Call(xval, yval))
        | false, ILExpr.Byte(xval), ILExpr.Byte(yval) ->
            ILExpr.Byte(op.Call(xval, yval))
        | false, ILExpr.UInt16(xval), ILExpr.UInt16(yval) ->
            ILExpr.UInt16(op.Call(xval, yval))
        | false, ILExpr.UInt32(xval), ILExpr.UInt32(yval) ->
            ILExpr.UInt32(op.Call(xval, yval))
        | false, ILExpr.UInt64(xval), ILExpr.UInt64(yval) ->
            ILExpr.UInt64(op.Call(xval, yval))
        | false, ILExpr.Int16(xval), ILExpr.Int16(yval) ->
            ILExpr.Int16(op.Call(xval, yval))
        | false, ILExpr.Int32(xval), ILExpr.Int32(yval) ->
            ILExpr.Int32(op.Call(xval, yval))
        | false, ILExpr.Int64(xval), ILExpr.Int64(yval) ->
            ILExpr.Int64(op.Call(xval, yval))
        | false, ILExpr.Single(xval), ILExpr.Single(yval) ->
            ILExpr.Single((op.Call(xval, yval)))            
        | false, ILExpr.Double(xval), ILExpr.Double(yval) ->
            ILExpr.Double((op.Call(xval, yval)))
        | _ -> ILExpr.NumericBinop(cked, op, x, y, ty)    
    | ILExpr.StaticCall(mi, args) ->
        let args = args |> List.map optimizeExpr
        match args with
        | [ILExpr.String(xval); ILExpr.String(yval)] when mi.DeclaringType = typeof<string> && mi.Name = "Concat" -> //i.e. "asdf" + "asdf" (can refactor this better?)
            ILExpr.String(xval + yval)
        | _ ->
            ILExpr.StaticCall(mi, args)
    | ILExpr.InstanceCall(instance, mi, args) ->
        let instance = optimizeExpr instance
        let args = args |> List.map optimizeExpr
        ILExpr.InstanceCall(instance, mi, args)
    | ILExpr.ComparisonBinop(op, x, y) -> //comparison constants folding
        let x, y = optimizeExpr x, optimizeExpr y
        match x, y with
        | ILExpr.Byte(xval), ILExpr.Byte(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.SByte(xval), ILExpr.SByte(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.Int16(xval), ILExpr.Int16(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.Int32(xval), ILExpr.Int32(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.Int64(xval), ILExpr.Int64(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.UInt16(xval), ILExpr.UInt16(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.UInt32(xval), ILExpr.UInt32(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.UInt64(xval), ILExpr.UInt64(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.Single(xval), ILExpr.Single(yval) ->
            ILExpr.Bool(op.Call(xval, yval))            
        | ILExpr.Double(xval), ILExpr.Double(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | ILExpr.Bool(xval), ILExpr.Bool(yval) ->
            ILExpr.Bool(op.Call(xval, yval))
        | _ -> ILExpr.ComparisonBinop(op, x, y)
    | ILExpr.Coerce(cked, x, ty) -> //mostly for implicit coersions to improve constants folding
        let x = optimizeExpr x
        match cked, x with //todo: these need to be expanded.
        | false, Int32(x) when ty = typeof<Double> -> ILExpr.Double(double x)
        | false, Double(x) when ty = typeof<Int32> -> ILExpr.Int32(int32 x)
        | _ -> ILExpr.Coerce(cked, x, ty)
    | ILExpr.LogicalNot(x) ->
        let x = optimizeExpr x
        match x with
        | ILExpr.Bool(true) -> ILExpr.Bool(false)
        | ILExpr.Bool(false) -> ILExpr.Bool(true)
        | _ -> ILExpr.LogicalNot(x)
    | ILExpr.Sequential(x,y,ty) ->
        let x,y = optimizeExpr x, optimizeExpr y
        match x, y with
        | ILExpr.Nop, ILExpr.Nop -> ILExpr.Nop //();() -> ()
        | ILExpr.Nop, _ -> y // (); exp -> exp
        //the following dead code optimization attempt can change the semantic meaning of the ILExpr resulting in invalid IL
        //| (ILExpr.Break | ILExpr.Continue | ILExpr.Throw(_)), _ -> x
        | _,_ -> ILExpr.Sequential(x,y,ty)
    | ILExpr.UMinus(cked, x, ty) ->
        let x = optimizeExpr x
        match cked, x with
        //TODO: other numeric literals
        | false, ILExpr.SByte(xval) -> ILExpr.SByte(-xval)
        | false, ILExpr.Int16(xval) -> ILExpr.Int16(-xval)
        | false, ILExpr.Int32(xval) -> ILExpr.Int32(-xval)
        | false, ILExpr.Int64(xval) -> ILExpr.Int64(-xval)
        | false, ILExpr.Single(xval) -> ILExpr.Single(-xval)
        | false, ILExpr.Double(xval) -> ILExpr.Double(-xval)
        | _ -> ILExpr.UMinus(cked, x, ty)
    | ILExpr.WhileLoop(cond, body) ->
        let cond = optimizeExpr cond
        match cond with
        | ILExpr.Bool(false) ->
            ILExpr.Nop
        | _ ->
            let body = optimizeExpr body
            ILExpr.WhileLoop(cond, body)
    | ILExpr.Cast(x, ty) ->
        ILExpr.Cast(optimizeExpr x, ty)
    | ILExpr.Ctor(ci, args, ty) ->
        ILExpr.Ctor(ci, List.map optimizeExpr args, ty)
    | ILExpr.Let(name, assign, body, ty) ->
        ILExpr.Let(name, optimizeExpr assign, optimizeExpr body, ty)
    | ILExpr.VarSet(name, assign) ->
        ILExpr.VarSet(name, optimizeExpr assign)
    | ILExpr.StaticFieldSet(fi, assign) ->
        ILExpr.StaticFieldSet(fi, optimizeExpr assign)
    | ILExpr.InstanceFieldGet(x, fi) ->
        ILExpr.InstanceFieldGet(optimizeExpr x, fi)
    | ILExpr.InstanceFieldSet(x,fi,y) ->
        ILExpr.InstanceFieldSet(optimizeExpr x, fi, optimizeExpr y)
    | Default(ty) ->
        match ty with
        | ByteTy -> ILExpr.Byte(Unchecked.defaultof<Byte>)
        | SByteTy -> ILExpr.SByte(Unchecked.defaultof<SByte>)
        | Int16Ty -> ILExpr.Int16(Unchecked.defaultof<Int16>)
        | Int32Ty -> ILExpr.Int32(Unchecked.defaultof<Int32>)
        | Int64Ty -> ILExpr.Int64(Unchecked.defaultof<Int64>)
        | UInt16Ty -> ILExpr.UInt16(Unchecked.defaultof<UInt16>)
        | UInt32Ty -> ILExpr.UInt32(Unchecked.defaultof<UInt32>)
        | UInt64Ty -> ILExpr.UInt64(Unchecked.defaultof<UInt64>)
        | SingleTy -> ILExpr.Single(Unchecked.defaultof<Single>)
        | DoubleTy -> ILExpr.Double(Unchecked.defaultof<Double>)
        | BooleanTy -> ILExpr.Bool(Unchecked.defaultof<Boolean>)
        | CharTy -> ILExpr.Char(Unchecked.defaultof<Char>)
        | _ -> exp
    | ILExpr.Throw(x) ->
        ILExpr.Throw(optimizeExpr x)
    | ILExpr.TryCatchFinally(tx, catchList, fx, ty) ->
        let tx = optimizeExpr tx

        let rec loop (catchList:(Type * string option * ILExpr) list) acc =
            match catchList with
            | [] -> acc
            | (filterTy, name, catch)::tl ->
                match acc with
                | (prevFilterTy:Type,_,_)::_ when prevFilterTy.IsAssignableFrom(filterTy) -> acc //this catch is unreachable, so return acc here
                | _ -> loop tl ((filterTy, name, (optimizeExpr catch))::acc)
        let catchList = loop catchList [] |> List.rev

        let fx = 
            match fx with
            | Some(fx) ->
                let fx = optimizeExpr fx
                match fx with
                | Nop -> None //finally block is nop, so we ignore it all together
                | _ -> Some(fx)
            | None -> None

        ILExpr.TryCatchFinally(tx, catchList, fx, ty)
    | Byte _  
    | SByte _ 
    | Int16 _ 
    | Int32 _ 
    | Int64 _ 
    | UInt16 _
    | UInt32 _
    | UInt64 _
    | Single _
    | Double _
    | String _
    | Char _
    | Bool _
    | Null _
    | Typeof _
    | StaticFieldGet _
    | VarGet _
    | Nop
    | Break
    | Continue 
    | Rethrow -> exp //atomic expressions
    | ILExpr.Error _ ->
        raise <| System.InvalidOperationException("Should not be optimizing an expression with errors")

let optimizeStmts stmts = 
    stmts
    |> List.map (fun x ->
        match x with
        | ILStmt.TypeDef(tyBuilder, tyinit, objinit) -> 
            ILStmt.TypeDef(tyBuilder, List.map optimizeExpr tyinit, List.map optimizeExpr objinit)
        | ILStmt.Error -> 
            raise <| System.InvalidOperationException("Should not be optimizing a statement with errors"))