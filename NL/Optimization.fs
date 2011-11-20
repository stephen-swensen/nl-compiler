module Swensen.NL.Optimization

let rec optimize (exp:texp) = 
    match exp with
    | texp.IfThenElse(condition, thenBranch, elseBranch, ty) -> //unreachable code elimination
        let condition, thenBranch, elseBranch = optimize condition, optimize thenBranch, optimize elseBranch
        match condition with
        | texp.Bool(true) -> thenBranch
        | texp.Bool(false) -> elseBranch
        | _ -> 
            match thenBranch with
            | texp.Nop -> texp.IfThen(condition, thenBranch) //asser ty = typeof<void>
            | _ ->
                texp.IfThenElse(condition, thenBranch, elseBranch, ty)
    | texp.IfThen(condition, thenBranch) -> //unreachable code elimination
        let condition, thenBranch = optimize condition, optimize thenBranch
        match condition with
        | texp.Bool(true) -> thenBranch
        | texp.Bool(false) -> texp.Nop
        | _ -> 
            match thenBranch with
            | texp.Nop -> condition
            | _ ->
                texp.IfThen(condition, thenBranch)
    | texp.NumericBinop(op, x, y, ty) -> //numeric constants folding
        let x, y = optimize x, optimize y
        match x, y with
        | texp.Int32(xval), texp.Int32(yval) ->
            texp.Int32(op.Call(xval, yval))
        | texp.Double(xval), texp.Double(yval) ->
            texp.Double((op.Call(xval, yval)))
        | _ -> texp.NumericBinop(op, x, y, ty)    
    | texp.StaticCall(mi, args, ty) ->
        let args = args |> List.map optimize
        match args with
        | [texp.String(xval); texp.String(yval)] when mi.DeclaringType = typeof<string> && mi.Name = "Concat" -> //i.e. "asdf" + "asdf" (can refactor this better?)
            texp.String(xval + yval)
        | _ ->
            texp.StaticCall(mi, args, ty)
    | texp.InstanceCall(instance, mi, args, ty) ->
        let instance = optimize instance
        let args = args |> List.map optimize
        texp.InstanceCall(instance, mi, args, ty)
    | texp.ComparisonBinop(op, x, y) -> //comparison constants folding
        let x, y = optimize x, optimize y
        match x, y with
        | texp.Int32(xval), texp.Int32(yval) ->
            texp.Bool(op.Call(xval, yval))
        | texp.Double(xval), texp.Double(yval) ->
            texp.Bool(op.Call(xval, yval))
        | texp.Bool(xval), texp.Bool(yval) ->
            texp.Bool(op.Call(xval, yval))
        | _ -> texp.ComparisonBinop(op, x, y)
    | texp.Coerce(x, ty) -> //mostly for implicit coersions to improve constants folding
        let x = optimize x
        match x with
        | Int32(x) -> Double(float x)
        | _ -> texp.Coerce(x, ty)
    | texp.LogicalNot(x) ->
        let x = optimize x
        match x with
        | texp.Bool(true) -> texp.Bool(false)
        | texp.Bool(false) -> texp.Bool(true)
        | _ -> texp.LogicalNot(x)
    | texp.Sequential(x,y,ty) ->
        let x,y = optimize x, optimize y
        match x, y with
        | texp.Nop, texp.Nop -> texp.Nop //();() -> ()
        | texp.Nop, _ -> y // (); exp -> exp
        | _,_ -> texp.Sequential(x,y,ty)
    | texp.UMinus(x, ty) ->
        let x = optimize x
        match x with
        | texp.Int32(xval) -> texp.Int32(-xval)
        | texp.Double(xval) -> texp.Double(-xval)
        | _ -> texp.UMinus(x, ty)
    | texp.WhileLoop(cond, body) ->
        let cond = optimize cond
        match cond with
        | texp.Bool(false) ->
            texp.Nop
        | _ ->
            let body = optimize body
            texp.WhileLoop(cond, body)
    | texp.Cast(x, ty) ->
        texp.Cast(optimize x, ty)
    | texp.Ctor(ci, args, ty) ->
        texp.Ctor(ci, List.map optimize args, ty)
    | texp.Let(name, assign, body, ty) ->
        texp.Let(name, optimize assign, optimize body, ty)
    | texp.VarSet(name, assign) ->
        texp.VarSet(name, optimize assign)
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
    | Error _ ->
        failwith "Should not be optimizing an ast with errors"