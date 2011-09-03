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
    ///bind a variable
    | Let              of string * rexp * rexp * Position
    ///reference a variable
    | Var              of string * Position
    ///call instance method on a variable or call a static method or call a constructor
    | NameCall         of string * rexp list * Position
    ///call instance method on an expression
    | ExpCall          of rexp * string * rexp list * Position
    ///discard left hand side, return right hand side
    | Sequential       of rexp * rexp * Position
    ///open a namespace
    | Open             of string * rexp * Position
    ///reference an assembly by name or dll path
    | Ref              of string * rexp * Position