module Swensen.Calc.Ast
open System

type binop =
    | Plus | Minus | Times | Div | Pow

module UT =
    type exp =
        | Double   of float
        | Int32    of int
        | Binop    of binop * exp * exp
        | UMinus   of exp
        | Fact     of exp

open UT

type typedExp =
    | Double   of float * Type
    | Int32    of int * Type
    | Binop    of binop * typedExp * typedExp * Type
    | UMinus   of typedExp * Type
    | Fact     of typedExp * Type
    with 
        member this.Type =
            match this with
            | Double(_,ty) -> ty
            | Int32(_,ty)
            | Binop(_,_,_,ty)
            | UMinus(_,ty)
            | Fact(_,ty) -> ty

let rec tycheck exp =
    match exp with
    | exp.Double(x) -> Double(x,typeof<float>)
    | exp.Int32(x) -> Int32(x,typeof<int>)
    | exp.UMinus(x) ->
        let x = tycheck x
        UMinus(x,x.Type)
    | exp.Fact(x) ->
        let x = tycheck x
        if x.Type <> typeof<int> then
            failwithf "factorial expects int but got: %A" x.Type
        else
            Fact(x, x.Type)
    | exp.Binop(op,x,y) ->
        let x, y = tycheck x, tycheck y
        let ty =
            if x.Type = typeof<int> && y.Type = typeof<int> then
                typeof<int>
            elif x.Type = typeof<float> || y.Type = typeof<float> then
                typeof<float>
            else
                failwithf "binop expects float or int args but got lhs=%A, rhs=%A" x.Type y.Type 
        Binop(op,x,y,ty)