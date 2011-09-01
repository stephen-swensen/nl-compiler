namespace Swensen.NewLang

open System

type numericBinop = Plus | Minus | Times | Div
type logicBinop = And | Or | ExOr

///Raw (untyped) parsed expression
type rexp =
    | Double           of float
    | Int32            of int
    | String           of string
    | NumericBinop     of numericBinop * rexp * rexp
    | Concat           of rexp * rexp
    | Pow              of rexp * rexp
    | UMinus           of rexp
    | Fact             of rexp
    | Let              of string * rexp * rexp
    | Var              of string
    | IdCall           of string * rexp list
    | ExpCall          of rexp * string * rexp list
    | Sequential       of rexp * rexp
    | Char             of char