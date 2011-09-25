namespace Swensen.NewLang

open System
open Microsoft.FSharp.Text.Lexing

type numericBinop = Plus | Minus | Times | Div
type logicBinop = And | Or | XOr
type comparisonBinop = Eq | Lt | Gt

type tySig =
    | TySig of string * tySig list

///Raw (untyped) parsed expression
type rexp =
    | Double           of float
    | Int32            of int
    | String           of string
    | Char             of char
    | Bool             of bool
    | Null             of tySig * PositionRange
    | Typeof           of tySig * PositionRange
    | NumericBinop     of numericBinop * rexp * rexp * PositionRange
    | Pow              of rexp * rexp * PositionRange
    | UMinus           of rexp * PositionRange
    | Fact             of rexp * PositionRange
    ///bind a variable
    | Let              of string * rexp * rexp * PositionRange
    ///reference a variable
    | Var              of string * PositionRange
    ///call instance method on a variable or call a static method or call a constructor
    | NameCall         of string * tySig list * rexp list * PositionRange
    ///static type name * static type generic args * method name * (optional) method generic args * method args * position
    | GenericTypeStaticCall of string * tySig list * string * tySig list * rexp list * PositionRange
    ///call instance method on an expression
    ///instance expresion * instance method name * (optional) generic type args * method arguments * pos info
    | ExpCall          of rexp * string * tySig list * rexp list * PositionRange
    ///discard left hand side, return right hand side
    | Sequential       of rexp * rexp * PositionRange
    ///open a namespace
    | Open             of string * rexp * PositionRange
    ///reference an assembly by name or dll path
    | Ref              of string * rexp * PositionRange
    | Not              of rexp * PositionRange
    | Cast             of rexp * tySig * PositionRange
    | IfThenElse       of rexp * rexp * rexp option * PositionRange //should be pos for each!
    | ComparisonBinop  of comparisonBinop * rexp * rexp * PositionRange
    | Nop              of PositionRange
    | VarSet           of string * rexp * PositionRange
    | WhileLoop        of rexp * rexp * PositionRange
    | Break            of PositionRange
    | Continue         of PositionRange
    with
        static member Or(lhs:rexp, rhs:rexp, pos:PositionRange) =
            rexp.IfThenElse(lhs, rexp.Bool(true), Some(rhs), pos)
        static member NotEq(lhs:rexp, rhs:rexp, pos:PositionRange) =
            rexp.Not(rexp.ComparisonBinop(Eq, lhs, rhs, pos), pos)
        static member LtEq(lhs:rexp, rhs:rexp, pos:PositionRange) =
            rexp.Or(rexp.ComparisonBinop(Lt, lhs, rhs, pos), rexp.ComparisonBinop(Eq, lhs, rhs, pos), pos)
        static member GtEq(lhs:rexp, rhs:rexp, pos:PositionRange) =
            rexp.Or(rexp.ComparisonBinop(Gt, lhs, rhs, pos), rexp.ComparisonBinop(Eq, lhs, rhs, pos), pos)
        static member Xor(lhs:rexp, rhs:rexp, pos:PositionRange) =
            rexp.IfThenElse(lhs, rexp.Not(rhs, pos), Some(rhs), pos)
        static member And(lhs:rexp, rhs:rexp, pos:PositionRange) =
            rexp.IfThenElse(lhs, rhs, Some(rexp.Bool(false)), pos)


            