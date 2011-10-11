module Swensen.NewLang.Optimization

let rec optimize (exp:texp) = 
    match exp with
    | texp.IfThenElse(condition, thenBranch, elseBranch, ty) ->
        let condition = optimize condition
        match condition with
        | texp.Bool(true) -> thenBranch
        | texp.Bool(false) -> elseBranch
        | _ -> texp.IfThenElse(condition, thenBranch, elseBranch, ty)
    | texp.NumericBinop(op, x, y, ty) ->
        let x, y = optimize x, optimize y
        match x, y with
        | texp.Int32(xval), texp.Int32(yval) ->
            let fsOp =
                match op with
                | numericBinop.Plus -> (+)
                | numericBinop.Div -> (/)
                | numericBinop.Minus -> (-)
                | numericBinop.Times -> (*)
            texp.Int32(fsOp xval yval)
        | texp.Double(xval), texp.Double(yval) ->
            let fsOp =
                match op with
                | numericBinop.Plus -> (+)
                | numericBinop.Div -> (/)
                | numericBinop.Minus -> (-)
                | numericBinop.Times -> (*)
            texp.Double(fsOp xval yval)
        | _ -> texp.NumericBinop(op, x, y, ty)
    | _ -> exp