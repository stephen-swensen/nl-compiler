module Swensen.Calc.Ast
open System

type value =
    | Rational of float
    | Integer  of int

type exp =
    | Value    of value
    | Plus     of exp * exp
    | Minus    of exp * exp
    | Times    of exp * exp
    | Div      of exp * exp
    | Pow      of exp * exp
    | UMinus   of exp
    | Fact     of exp