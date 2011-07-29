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
        | Let      of string * exp * exp

type exp =
    | Double   of float * Type
    | Int32    of int * Type
    | Binop    of binop * exp * exp * Type
    | UMinus   of exp * Type
    | Fact     of exp * Type
    | Let      of string * exp * exp * Type
    with 
        member this.Type =
            match this with
            | Double(_,ty)
            | Int32(_,ty)
            | Binop(_,_,_,ty)
            | UMinus(_,ty)
            | Fact(_,ty) 
            | Let(_,_,_,ty) -> ty

let rec tycheck exp =
    match exp with
    | UT.Double(x) -> Double(x,typeof<float>)
    | UT.Int32(x) -> Int32(x,typeof<int>)
    | UT.UMinus(x) ->
        let x = tycheck x
        UMinus(x,x.Type)
    | UT.Fact(x) ->
        let x = tycheck x
        if x.Type <> typeof<int> then
            failwithf "factorial expects int but got: %A" x.Type
        else
            Fact(x, x.Type)
    | UT.Binop(op,x,y) ->
        let x, y = tycheck x, tycheck y
        let ty =
            if x.Type = typeof<int> && y.Type = typeof<int> then
                typeof<int>
            elif x.Type = typeof<float> || y.Type = typeof<float> then
                typeof<float>
            else
                failwithf "binop expects float or int args but got lhs=%A, rhs=%A" x.Type y.Type 
        Binop(op,x,y,ty)
    | UT.Let(id, assign, body) ->
        let assign, body = tycheck assign, tycheck body
        Let(id,assign, body, body.Type)