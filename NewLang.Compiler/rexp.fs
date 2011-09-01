namespace Swensen.NewLang

open System
open Microsoft.FSharp.Text.Lexing

type numericBinop = Plus | Minus | Times | Div
type logicBinop = And | Or | ExOr

///Raw (untyped) parsed expression
type rexp =
    | Double           of float
    | Int32            of int
    | String           of string
    | Char             of char
    | NumericBinop     of numericBinop * rexp * rexp * Position
    | Concat           of rexp * rexp * Position
    | Pow              of rexp * rexp * Position
    | UMinus           of rexp * Position
    | Fact             of rexp * Position
    | Let              of string * rexp * rexp * Position
    | Var              of string * Position
    | NameCall         of string * rexp list * Position
    | ExpCall          of rexp * string * rexp list * Position
    | Sequential       of rexp * rexp * Position