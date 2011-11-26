module Swensen.NL.Optimization

let optimize (tn:tnl) =
    let rec optimizeExp exp = 
        match exp with
        | texp.IfThenElse(condition, thenBranch, elseBranch, ty) -> //unreachable code elimination
            let condition, thenBranch, elseBranch = optimizeExp condition, optimizeExp thenBranch, optimizeExp elseBranch
            match condition with
            | texp.Bool(true) -> thenBranch
            | texp.Bool(false) -> elseBranch
            | _ -> 
                match thenBranch with
                | texp.Nop -> texp.IfThen(condition, thenBranch) //asser ty = typeof<void>
                | _ ->
                    texp.IfThenElse(condition, thenBranch, elseBranch, ty)
        | texp.IfThen(condition, thenBranch) -> //unreachable code elimination
            let condition, thenBranch = optimizeExp condition, optimizeExp thenBranch
            match condition with
            | texp.Bool(true) -> thenBranch
            | texp.Bool(false) -> texp.Nop
            | _ -> 
                match thenBranch with
                | texp.Nop -> condition
                | _ ->
                    texp.IfThen(condition, thenBranch)
        | texp.NumericBinop(op, x, y, ty) -> //numeric constants folding
            let x, y = optimizeExp x, optimizeExp y
            match x, y with
            | texp.Int32(xval), texp.Int32(yval) ->
                texp.Int32(op.Call(xval, yval))
            | texp.Double(xval), texp.Double(yval) ->
                texp.Double((op.Call(xval, yval)))
            | _ -> texp.NumericBinop(op, x, y, ty)    
        | texp.StaticCall(mi, args, ty) ->
            let args = args |> List.map optimizeExp
            match args with
            | [texp.String(xval); texp.String(yval)] when mi.DeclaringType = typeof<string> && mi.Name = "Concat" -> //i.e. "asdf" + "asdf" (can refactor this better?)
                texp.String(xval + yval)
            | _ ->
                texp.StaticCall(mi, args, ty)
        | texp.InstanceCall(instance, mi, args, ty) ->
            let instance = optimizeExp instance
            let args = args |> List.map optimizeExp
            texp.InstanceCall(instance, mi, args, ty)
        | texp.ComparisonBinop(op, x, y) -> //comparison constants folding
            let x, y = optimizeExp x, optimizeExp y
            match x, y with
            | texp.Int32(xval), texp.Int32(yval) ->
                texp.Bool(op.Call(xval, yval))
            | texp.Double(xval), texp.Double(yval) ->
                texp.Bool(op.Call(xval, yval))
            | texp.Bool(xval), texp.Bool(yval) ->
                texp.Bool(op.Call(xval, yval))
            | _ -> texp.ComparisonBinop(op, x, y)
        | texp.Coerce(x, ty) -> //mostly for implicit coersions to improve constants folding
            let x = optimizeExp x
            match x with
            | Int32(x) -> Double(float x)
            | _ -> texp.Coerce(x, ty)
        | texp.LogicalNot(x) ->
            let x = optimizeExp x
            match x with
            | texp.Bool(true) -> texp.Bool(false)
            | texp.Bool(false) -> texp.Bool(true)
            | _ -> texp.LogicalNot(x)
        | texp.Sequential(x,y,ty) ->
            let x,y = optimizeExp x, optimizeExp y
            match x, y with
            | texp.Nop, texp.Nop -> texp.Nop //();() -> ()
            | texp.Nop, _ -> y // (); exp -> exp
            | _,_ -> texp.Sequential(x,y,ty)
        | texp.UMinus(x, ty) ->
            let x = optimizeExp x
            match x with
            | texp.Int32(xval) -> texp.Int32(-xval)
            | texp.Double(xval) -> texp.Double(-xval)
            | _ -> texp.UMinus(x, ty)
        | texp.WhileLoop(cond, body) ->
            let cond = optimizeExp cond
            match cond with
            | texp.Bool(false) ->
                texp.Nop
            | _ ->
                let body = optimizeExp body
                texp.WhileLoop(cond, body)
        | texp.Cast(x, ty) ->
            texp.Cast(optimizeExp x, ty)
        | texp.Ctor(ci, args, ty) ->
            texp.Ctor(ci, List.map optimizeExp args, ty)
        | texp.Let(name, assign, body, ty) ->
            texp.Let(name, optimizeExp assign, optimizeExp body, ty)
        | texp.VarSet(name, assign) ->
            texp.VarSet(name, optimizeExp assign)
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
        | texp.Error _ ->
            failwith "Should not be optimizing an expression with errors"

    match tn with
    | tnl.Exp(x) -> optimizeExp x |> tnl.Exp
    | tnl.StmtList(xl) ->
        xl 
        |> List.map (fun x ->
            match x with
            | tstmt.Do(x) -> optimizeExp x |> tstmt.Do
            | tstmt.Let(name, x) -> 
                let x = optimizeExp x
                tstmt.Let(name, x))
        |> tnl.StmtList
    | tnl.Error _ ->
        failwith "Should not be optimizing an NL fragment with errors"