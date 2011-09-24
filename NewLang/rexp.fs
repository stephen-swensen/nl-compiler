﻿namespace Swensen.NewLang

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
    | Null             of tySig * Position
    | Typeof           of tySig * Position
    | NumericBinop     of numericBinop * rexp * rexp * Position
    | Pow              of rexp * rexp * Position
    | UMinus           of rexp * Position
    | Fact             of rexp * Position
    ///bind a variable
    | Let              of string * rexp * rexp * Position
    ///reference a variable
    | Var              of string * Position
    ///call instance method on a variable or call a static method or call a constructor
    | NameCall         of string * tySig list * rexp list * Position
    ///static type name * static type generic args * method name * (optional) method generic args * method args * position
    | GenericTypeStaticCall of string * tySig list * string * tySig list * rexp list * Position
    ///call instance method on an expression
    ///instance expresion * instance method name * (optional) generic type args * method arguments * pos info
    | ExpCall          of rexp * string * tySig list * rexp list * Position
    ///discard left hand side, return right hand side
    | Sequential       of rexp * rexp * Position
    ///open a namespace
    | Open             of string * rexp * Position
    ///reference an assembly by name or dll path
    | Ref              of string * rexp * Position
    | Not              of rexp * Position
    | Cast             of rexp * tySig * Position
    | IfThenElse       of rexp * rexp * rexp option * Position //should be pos for each!
    | ComparisonBinop  of comparisonBinop * rexp * rexp * Position
    | Nop              of Position
    | VarSet           of string * rexp * Position
    | WhileLoop        of rexp * rexp * Position
    | Break            of Position
    | Continue         of Position
    with
        static member Or(lhs:rexp, rhs:rexp, pos:Position) =
            rexp.IfThenElse(lhs, rexp.Bool(true), Some(rhs), pos)
        static member NotEq(lhs:rexp, rhs:rexp, pos:Position) =
            rexp.Not(rexp.ComparisonBinop(Eq, lhs, rhs, pos), pos)
        static member LtEq(lhs:rexp, rhs:rexp, pos:Position) =
            rexp.Or(rexp.ComparisonBinop(Lt, lhs, rhs, pos), rexp.ComparisonBinop(Eq, lhs, rhs, pos), pos)
        static member GtEq(lhs:rexp, rhs:rexp, pos:Position) =
            rexp.Or(rexp.ComparisonBinop(Gt, lhs, rhs, pos), rexp.ComparisonBinop(Eq, lhs, rhs, pos), pos)
        static member Xor(lhs:rexp, rhs:rexp, pos:Position) =
            rexp.IfThenElse(lhs, rexp.Not(rhs, pos), Some(rhs), pos)
        static member And(lhs:rexp, rhs:rexp, pos:Position) =
            rexp.IfThenElse(lhs, rhs, Some(rexp.Bool(false)), pos)


            