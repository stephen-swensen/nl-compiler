module Swensen.Calc.Ast
open System

type value =
    | Rational of float
    | Integer  of int

type exp =
    | Value    of value * (Type ref)
    | Plus     of exp * exp * (Type ref)
    | Minus    of exp * exp * (Type ref)
    | Times    of exp * exp * (Type ref)
    | Div      of exp * exp * (Type ref)
    | Pow      of exp * exp * (Type ref)
    | UMinus   of exp * (Type ref)
    | Fact     of exp //always integer
    with
        member this.Type =
            match this with
            | Value(_,ty) 
            | Plus(_,_,ty) 
            | Minus(_,_,ty) 
            | Times(_,_,ty) 
            | Div(_,_,ty)
            | Pow(_,_,ty)
            | UMinus(_,ty) -> !ty
            | Fact _ -> typeof<int>