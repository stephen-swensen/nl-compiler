module Swensen.NL.Optimization
open Swensen.NL.Ail

let optimize (tn:ILTopLevel) =
    let rec optimizeExp exp = 
        match exp with
        | ILExpr.IfThenElse(condition, thenBranch, elseBranch, ty) -> //unreachable code elimination
            let condition, thenBranch, elseBranch = optimizeExp condition, optimizeExp thenBranch, optimizeExp elseBranch
            match condition with
            | ILExpr.Bool(true) -> thenBranch
            | ILExpr.Bool(false) -> elseBranch
            | _ -> 
                match thenBranch with
                | ILExpr.Nop -> ILExpr.IfThen(condition, thenBranch) //asser ty = typeof<void>
                | _ ->
                    ILExpr.IfThenElse(condition, thenBranch, elseBranch, ty)
        | ILExpr.IfThen(condition, thenBranch) -> //unreachable code elimination
            let condition, thenBranch = optimizeExp condition, optimizeExp thenBranch
            match condition with
            | ILExpr.Bool(true) -> thenBranch
            | ILExpr.Bool(false) -> ILExpr.Nop
            | _ -> 
                match thenBranch with
                | ILExpr.Nop -> condition
                | _ ->
                    ILExpr.IfThen(condition, thenBranch)
        | ILExpr.NumericBinop(op, x, y, ty) -> //numeric constants folding
            let x, y = optimizeExp x, optimizeExp y
            match x, y with
            | ILExpr.Int32(xval), ILExpr.Int32(yval) ->
                ILExpr.Int32(op.Call(xval, yval))
            | ILExpr.Double(xval), ILExpr.Double(yval) ->
                ILExpr.Double((op.Call(xval, yval)))
            | _ -> ILExpr.NumericBinop(op, x, y, ty)    
        | ILExpr.StaticCall(mi, args, ty) ->
            let args = args |> List.map optimizeExp
            match args with
            | [ILExpr.String(xval); ILExpr.String(yval)] when mi.DeclaringType = typeof<string> && mi.Name = "Concat" -> //i.e. "asdf" + "asdf" (can refactor this better?)
                ILExpr.String(xval + yval)
            | _ ->
                ILExpr.StaticCall(mi, args, ty)
        | ILExpr.InstanceCall(instance, mi, args, ty) ->
            let instance = optimizeExp instance
            let args = args |> List.map optimizeExp
            ILExpr.InstanceCall(instance, mi, args, ty)
        | ILExpr.ComparisonBinop(op, x, y) -> //comparison constants folding
            let x, y = optimizeExp x, optimizeExp y
            match x, y with
            | ILExpr.Int32(xval), ILExpr.Int32(yval) ->
                ILExpr.Bool(op.Call(xval, yval))
            | ILExpr.Double(xval), ILExpr.Double(yval) ->
                ILExpr.Bool(op.Call(xval, yval))
            | ILExpr.Bool(xval), ILExpr.Bool(yval) ->
                ILExpr.Bool(op.Call(xval, yval))
            | _ -> ILExpr.ComparisonBinop(op, x, y)
        | ILExpr.Coerce(x, ty) -> //mostly for implicit coersions to improve constants folding
            let x = optimizeExp x
            match x with
            | Int32(x) when ty = typeof<double> -> Double(double x)
            | Double(x) when ty = typeof<int32> -> Int32(int32 x)
            | _ -> ILExpr.Coerce(x, ty)
        | ILExpr.LogicalNot(x) ->
            let x = optimizeExp x
            match x with
            | ILExpr.Bool(true) -> ILExpr.Bool(false)
            | ILExpr.Bool(false) -> ILExpr.Bool(true)
            | _ -> ILExpr.LogicalNot(x)
        | ILExpr.Sequential(x,y,ty) ->
            let x,y = optimizeExp x, optimizeExp y
            match x, y with
            | ILExpr.Nop, ILExpr.Nop -> ILExpr.Nop //();() -> ()
            | ILExpr.Nop, _ -> y // (); exp -> exp
            | _,_ -> ILExpr.Sequential(x,y,ty)
        | ILExpr.UMinus(x, ty) ->
            let x = optimizeExp x
            match x with
            | ILExpr.Int32(xval) -> ILExpr.Int32(-xval)
            | ILExpr.Double(xval) -> ILExpr.Double(-xval)
            | _ -> ILExpr.UMinus(x, ty)
        | ILExpr.WhileLoop(cond, body) ->
            let cond = optimizeExp cond
            match cond with
            | ILExpr.Bool(false) ->
                ILExpr.Nop
            | _ ->
                let body = optimizeExp body
                ILExpr.WhileLoop(cond, body)
        | ILExpr.Cast(x, ty) ->
            ILExpr.Cast(optimizeExp x, ty)
        | ILExpr.Ctor(ci, args, ty) ->
            ILExpr.Ctor(ci, List.map optimizeExp args, ty)
        | ILExpr.Let(name, assign, body, ty) ->
            ILExpr.Let(name, optimizeExp assign, optimizeExp body, ty)
        | ILExpr.VarSet(name, assign) ->
            ILExpr.VarSet(name, optimizeExp assign)
        | ILExpr.StaticFieldSet(fi, assign) ->
            ILExpr.StaticFieldSet(fi, optimizeExp assign)
        | ILExpr.StaticFieldGet _
        | Double _
        | Int32 _
        | String _
        | Char _
        | Bool _
        | Null _
        | Typeof _
        | Var _
        | Default _
        | Nop
        | Break         
        | Continue -> exp //atomic expressions
        | ILExpr.Error _ ->
            failwith "Should not be optimizing an expression with errors"

    match tn with
    | ILTopLevel.Exp(x) -> optimizeExp x |> ILTopLevel.Exp
    | ILTopLevel.StmtList(xl) ->
        xl 
        |> List.map (fun x ->
            match x with
            | ILStmt.Do(x) -> optimizeExp x |> ILStmt.Do
            | ILStmt.Let(name, x) -> 
                let x = optimizeExp x
                ILStmt.Let(name, x))
        |> ILTopLevel.StmtList
    | ILTopLevel.Error _ ->
        failwith "Should not be optimizing an NL fragment with errors"