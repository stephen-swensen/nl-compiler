namespace Swensen.NewLang

open System
open Microsoft.FSharp.Text.Lexing

type numericBinop = Plus | Minus | Times | Div
type logicBinop = And | Or | XOr
type comparisonOps = Eq | NotEq | Lt | Gt | LtEq | GtEq

type genericSig =
    | NonGeneric of string
    | Generic of string * genericSig list

///Raw (untyped) parsed expression
type rexp =
    | Double           of float
    | Int32            of int
    | String           of string
    | Char             of char
    | Bool             of bool
    | Null             of genericSig * Position
    | Typeof           of genericSig * Position
//    | LogicBinop       of logicBinop * rexp * rexp * Position
//    | Not              of rexp * Position
    | NumericBinop     of numericBinop * rexp * rexp * Position
    | Pow              of rexp * rexp * Position
    | UMinus           of rexp * Position
    | Fact             of rexp * Position
    ///bind a variable
    | Let              of string * rexp * rexp * Position
    ///reference a variable
    | Var              of string * Position
    ///call instance method on a variable or call a static method or call a constructor
    | NameCall         of string * (genericSig list) option * rexp list * Position
    ///static type name * static type generic args * method name * (optional) method generic args * method args * position
    | GenericTypeStaticCall of string * genericSig list * string * (genericSig list) option * rexp list * Position
    ///call instance method on an expression
    ///instance expresion * instance method name * (optional) generic type args * method arguments * pos info
    | ExpCall          of rexp * string * (genericSig list) option * rexp list * Position
    ///discard left hand side, return right hand side
    | Sequential       of rexp * rexp * Position
    ///open a namespace
    | Open             of string * rexp * Position
    ///reference an assembly by name or dll path
    | Ref              of string * rexp * Position
    | Not              of rexp * Position