﻿module Swensen.NewLang.Optimization

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
    | texp.ComparisonBinop(op, x, y) -> //comparison constants folding
        let x, y = optimize x, optimize y
        match x, y with
        | texp.Int32(xval), texp.Int32(yval) ->
            texp.Bool(op.Call(xval, yval))
//        | texp.Double(xval), texp.Double(yval) ->
//            texp.Double((op.Calc(xval, yval)))
        | _ -> texp.ComparisonBinop(op, x, y)
    | texp.Coerce(x, ty) -> //mostly for implicit coersions to improve constants folding
        let x = optimize x
        match x with
        | Int32(x) -> Double(float x)
        | _ -> texp.Coerce(x, ty)
    | _ -> exp