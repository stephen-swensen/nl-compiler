namespace Swensen.NewLang

open System
open Microsoft.FSharp.Text.Lexing

type numericBinop = Plus | Minus | Times | Div
    with
        ///get the .NET method name corresponding to this op
        member x.Name =
            match x with
            | Plus -> "op_Addition"
            | Minus -> "op_Subtraction"
            | Div -> "op_Division"
            | Times -> "op_Multiply"

        ///get the NL textual symbol of this op
        member x.Symbol =
            match x with
            | Plus -> "+"
            | Minus -> "-"
            | Times -> "*"
            | Div -> "/"
        
        ///Call the F# analog to this operator on the operands
        member inline x.Call(lhs:'a,rhs:'a):'a =
            let fsop =
                match x with
                | numericBinop.Plus -> (+)
                | numericBinop.Div -> (/)
                | numericBinop.Minus -> (-)
                | numericBinop.Times -> (*)

            fsop lhs rhs


type logicBinop = And | Or
    with
        member x.Symbol =
            match x with
            | And -> "&&"
            | Or -> "||"

///For semantic analysis, we enumerate each case instead of making LtEq, GtEq, and Neq merely syntactic compound forms.
type rcomparisonBinop = Eq | Lt | Gt | LtEq | GtEq | Neq
    with
        member x.Symbol =
            match x with
            | Eq -> "=="
            | Lt -> "<"
            | Gt -> ">"
            | LtEq -> "<="
            | GtEq -> ">="
            | Neq -> "~="

        member x.Name =
            match x with
            | Eq -> "op_Equality"
            | Lt -> "op_LessThan"
            | Gt -> "op_GreaterThan"
            | LtEq -> "op_LessThanOrEqual"
            | GtEq -> "op_GreaterThanOrEqual"
            | Neq -> "op_Inequality"

//        ///Call the F# analog to this operator on the operands
//        member inline x.Call(lhs:'a,rhs:'a):bool =
//            let fsop =
//                match x with
//                | Eq -> (=)
//                | Lt -> (<)
//                | Gt -> (>)
//                | LtEq -> (<=)
//                | GtEq -> (>=)
//                | Neq -> (<>)
//
//            fsop lhs rhs

type tySig =
    | TySig of string * tySig list
    with 
        member x.Name =
            let rec build = function
                | TySig(name, []) -> name
                | TySig(name, xl) -> name + "[" + (xl |> List.map build |> String.concat ",") + "]"
            build x

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
    //| Fact             of rexp * PositionRange
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
    | OpenNamespace    of string * rexp * PositionRange
    ///reference an assembly by name or dll path
    | OpenAssembly     of string * rexp * PositionRange
    | LogicalNot       of rexp * PositionRange
    | Cast             of rexp * tySig * PositionRange
    | IfThenElse       of rexp * rexp * rexp option * PositionRange //should be pos for each!
    | ComparisonBinop  of rcomparisonBinop * rexp * rexp * PositionRange
    | Nop              of PositionRange
    | VarSet           of string * rexp * PositionRange
    | WhileLoop        of rexp * rexp * PositionRange
    | Break            of PositionRange
    | Continue         of PositionRange
    | LogicBinop       of logicBinop * (rexp * PositionRange) * (rexp * PositionRange)
//    with
//        static member Or(lhs:rexp, rhs:rexp, pos:PositionRange) =
//            rexp.IfThenElse(lhs, rexp.Bool(true), Some(rhs), pos)
//        static member And(lhs:rexp, rhs:rexp, pos:PositionRange) =
//            rexp.IfThenElse(lhs, rhs, Some(rexp.Bool(false)), pos)