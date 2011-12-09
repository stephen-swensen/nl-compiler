///Abstract Syntax datatypes
module Swensen.NL.Ast

open System
open Microsoft.FSharp.Text.Lexing

type SynNumericBinop = Plus | Minus | Times | Div
    with
        ///get the .NET method name corresponding to this op (used to get MethodInfo for non-primitive operands, excluding string concat which is handled separately)
        member x.Name =
            match x with
            | Plus -> "op_Addition"
            | Minus -> "op_Subtraction"
            | Div -> "op_Division"
            | Times -> "op_Multiply"

        ///get the NL textual symbol of this op (used for error messages)
        member x.Symbol =
            match x with
            | Plus -> "+"
            | Minus -> "-"
            | Times -> "*"
            | Div -> "/"
        
        ///Call the F# analog to this operator on the operands (used for constants folding during optimization)
        member inline x.Call(lhs:'a,rhs:'a):'a =
            let fsop =
                match x with
                | Plus -> (+)
                | Div -> (/)
                | Minus -> (-)
                | Times -> (*)

            fsop lhs rhs


type SynLogicBinop = And | Or
    with
        member x.Symbol =
            match x with
            | And -> "&&"
            | Or -> "||"

///For semantic analysis, we enumerate each case instead of making LtEq, GtEq, and Neq merely syntactic compound forms.
type SynComparisonBinop = Eq | Lt | Gt | LtEq | GtEq | Neq
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

type Path(path:string) =
    do
        if path |> String.IsNullOrEmpty then
            invalidArg "path" "can't be null or empty"

    let isShort, longPrefix, shortSuffix =
        let split = path.Split('.')
        match split.Length with
        | 1 -> true, "", path
        | _ ->
            false, String.Join(".",split.[..split.Length-2]), split.[split.Length-1]
    
    member this.LongPrefix = longPrefix
    member this.ShortSuffix = shortSuffix
    member this.IsShort = isShort
    member this.IsLong = not isShort    
    member this.Text = path
    override this.ToString() = this.Text
    member this.Parts =
        let split = path.Split('.')
        [
            for i in 1..split.Length do
                yield Path(String.Join(".", split.[0..i-1]))
        ]
    static member JoinShortSuffixes(paths:Path list) =
        Path(String.Join(".", paths |> Seq.map (fun i -> i.ShortSuffix)))

type TySig(genericName:string, genericArgs: TySig list, pos:PositionRange) =
    do
        if String.IsNullOrWhiteSpace genericName then
            invalidArg "genericName" "Cannot be null or whitespace"

    new (genericName:string, pos) = TySig(genericName,[], pos)
    new (genericName:Path, genericArgs, pos) = TySig(genericName.Text, genericArgs, pos)
    new (genericName:Path, pos) = TySig(genericName.Text,[], pos)
    
    ///i.e. Dictionary in Dictionary<'T, 'R>
    member x.GenericName = genericName
    ///i.e. String and Int in Dictionary<String, Int>
    member x.GenericArgs = genericArgs
    member x.Pos = pos
let (|TySig|) (tySig:TySig) =
    (tySig.GenericName, tySig.GenericArgs)
type TySig with
    ///i.e. Dictionary<String,Int>
    member x.Name =
        let rec build = function
            | TySig(name, []) -> name
            | TySig(name, xl) -> name + "[" + (xl |> List.map build |> String.concat ",") + "]"
        build x
    override x.ToString() =
        x.Name
    
//n.b. PositionRange convention is: 1) if position range applies to the entire expression,
//     then it is the last element in the tupled case, 2) if position range applies to a pariticular
//     sub-expression or token, then it is tupled with the subexpression or token
///Raw (untyped) parsed expression
type SynExpr =
    | Double           of float
    | Int32            of int
    | String           of string
    | Char             of char
    | Bool             of bool
    | Null             of TySig
    | Typeof           of TySig
    | Default          of TySig
    | NumericBinop     of SynNumericBinop * SynExpr * SynExpr * PositionRange
    | Pow              of SynExpr * SynExpr * PositionRange
    //TODO: implement semantic analysis
    | UMinus           of SynExpr * PositionRange
    //| Fact             of SynExpr * PositionRange
    ///bind a variable
    | Let              of string * (SynExpr * PositionRange) * SynExpr
    ///reference a variable
    | Var              of Path * PositionRange
    ///call instance method on a variable or call a static method or call a constructor
    | NameCall         of Path * TySig list * SynExpr list * PositionRange
    ///static type name * static type generic args * method name * (optional) method generic args * method args * position
    | GenericTypeStaticCall of string * TySig list * string * TySig list * SynExpr list * PositionRange
    ///call instance method on an expression
    ///instance expresion * instance method name * (optional) generic type args * method arguments * pos info
    | ExpCall          of SynExpr * string * TySig list * SynExpr list * PositionRange
    //| ExprDataMember    of SynExpr * (Path * PositionRange)
    ///discard left hand side, return right hand side
    | Sequential       of SynExpr * (SynExpr * PositionRange)
    ///open a namespace
    | OpenNamespaceOrType of TySig * SynExpr 
    ///reference an assembly by name or dll path
    | OpenAssembly     of (string * PositionRange) * SynExpr
    | LogicalNot       of SynExpr * PositionRange
    | Cast             of SynExpr * TySig * PositionRange
    | IfThenElse       of (SynExpr * PositionRange) * SynExpr * SynExpr option * PositionRange //should be pos for each!
    | ComparisonBinop  of SynComparisonBinop * SynExpr * SynExpr * PositionRange
    | Nop
    | VarSet           of (Path * PositionRange) * SynExpr * PositionRange
    | WhileLoop        of (SynExpr * PositionRange) * SynExpr
    | Break            of PositionRange
    | Continue         of PositionRange
    | LogicBinop       of SynLogicBinop * (SynExpr * PositionRange) * (SynExpr * PositionRange)

type SynStmt =
    | Let                   of string * (SynExpr * PositionRange)
    | OpenNamespaceOrType   of TySig
    | OpenAssembly          of string * PositionRange
    | Do                    of SynExpr

//represents a "raw" top level language element
type SynTopLevel =
    | Expr          of SynExpr
    | StmtList      of SynStmt list

