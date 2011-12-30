module Swensen.NL.Optimization
open Swensen.NL.Ail

//we do not optimize checked operations since that might result in a compile time overflow (in the 
//future we may have all constant expressions checked at compile time, like c#)
///Optimize the ILTopLevel tree. An exception will be raised if the tree contains any "errors".
let optimize (tl:ILTopLevel) =
    let rec optimizeExpr exp = 
        match exp with
        | ILExpr.IfThenElse(condition, thenBranch, elseBranch, ty) -> //unreachable code elimination
            let condition, thenBranch, elseBranch = optimizeExpr condition, optimizeExpr thenBranch, optimizeExpr elseBranch
            match condition with
            | ILExpr.Bool(true) -> thenBranch
            | ILExpr.Bool(false) -> elseBranch
            | _ -> 
                match thenBranch with
                | ILExpr.Nop -> ILExpr.IfThen(condition, thenBranch) //asser ty = typeof<void>
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
            match cked, x with
            | false, Int32(x) when ty = typeof<double> -> Double(double x)
            | false, Double(x) when ty = typeof<int32> -> Int32(int32 x)
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
            | _,_ -> ILExpr.Sequential(x,y,ty)
        | ILExpr.UMinus(cked, x, ty) ->
            let x = optimizeExpr x
            match cked, x with
            //TODO: other numeric literals
            | false, ILExpr.Int32(xval) -> ILExpr.Int32(-xval)
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
            if ty = typeof<int32> then
                ILExpr.Int32(Unchecked.defaultof<int32>)
            elif ty = typeof<double> then
                ILExpr.Double(Unchecked.defaultof<double>)
            elif ty = typeof<bool> then
                ILExpr.Bool(Unchecked.defaultof<bool>)
            elif ty = typeof<char> then
                ILExpr.Char(Unchecked.defaultof<char>)
            else
                exp        
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
        | Continue -> exp //atomic expressions
        | ILExpr.Error _ ->
            failwith "Should not be optimizing an expression with errors"

    match tl with
    | ILTopLevel.Expr(x) -> optimizeExpr x |> ILTopLevel.Expr
    | ILTopLevel.StmtList(xl) ->
        xl 
        |> List.map (fun x ->
            match x with
            | ILStmt.Do(x) -> optimizeExpr x |> ILStmt.Do
            | ILStmt.Let(name, x) -> 
                let x = optimizeExpr x
                ILStmt.Let(name, x))
        |> ILTopLevel.StmtList
    | ILTopLevel.Error _ ->
        failwith "Should not be optimizing a top-level NL fragment with errors"