module Swensen.NewLang.Optimization

let rec optimize (exp:texp) = 
    match exp with
    | texp.IfThenElse(condition, thenBranch, elseBranch, ty) -> //unreachable code elimination
        let condition = optimize condition
        match condition with
        | texp.Bool(true) -> thenBranch
        | texp.Bool(false) -> elseBranch
        | _ -> texp.IfThenElse(condition, thenBranch, elseBranch, ty)
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
        //todo: boolean and double folding
//        | texp.Double(xval), texp.Double(yval) ->
//            texp.Double((op.Calc(xval, yval)))
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
        | _ -> x
    | _ -> exp

//type MyStruct =
//    struct
//        val myInt : int = 
//        val myString : string
//    end

let x = defaultArg<int>