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

///A Path is a non-empty sequence of non-empty strings representing e.g. "a.b.c.d..."
type Path(parts:(string * PositionRange) seq) =
    do
        if parts = null then
            nullArg "parts" "must not be null"

        if parts |> Seq.isEmpty then
            invalidArg "parts" "must not be empty"

        if parts |> Seq.exists (fun (partText,_) -> String.IsNullOrWhiteSpace partText) then
            invalidArg "parts" "parts may not contain any null or whitespace part text"

    ///invariant: parts.Length >= 1
    let parts = parts |> Seq.toArray //easier to work with than list, but we still treat as immutable
    let pos = PositionRange(parts.[0] |> snd, parts.[parts.Length-1] |> snd)

    member this.Length = parts.Length
    member this.IsMultiPart = this.Length > 1
    member this.IsSinglePart = this.Length = 1
    member this.Pos = pos
    member this.Parts = parts |> Seq.readonly
    member this.Text = parts |> Seq.map fst |> String.concat "."
    override this.ToString() = this.Text
    
    new(part:string, pos:PositionRange) = Path(Seq.singleton (part,pos))
    new(single:string * PositionRange) = Path(Seq.singleton single)

    new(path:Path, last:string*PositionRange) = Path(seq { yield! path.Parts ; yield last }) 
    new(first:string*PositionRange, path:Path) = Path(seq { yield first ; yield! path.Parts })

    static member op_Addition(path:Path, last:string * PositionRange) = Path(path, last)
    static member op_Addition(first:string*PositionRange, path:Path) = Path(first, path)
        
    ///e.g. "a.b.c" -> "a.b". May be empty string if Path only contains one part (e.g. "a" -> "")
    member this.LeadingPartsText =
        if this.IsSinglePart then ""
        else
            parts.[0..parts.Length-2]
            |> Seq.map fst
            |> String.concat "."

    ///e.g. "a.b.c" -> Some("a.b"). May be None if Path only contains one part (e.g. "a" -> None)
    member this.LeadingPartsPath =
        if this.IsSinglePart then None
        else
            Some(Path(parts.[0..parts.Length-2]))

    ///e.g. "a.b.c" -> "c", will always be non-empty
    member this.LastPartText =
        parts.[parts.Length-1] |> fst

    ///e.g. "a.b.c" -> "c", will always be non-empty
    member this.LastPartPath =
        Path(parts.[parts.Length-1])

    member this.FirstPartPathWithRest = //TODO:TEST
        if this.IsSinglePart then this, None
        else Path([parts.[0]]), Some(Path(parts.[1..parts.Length-1]))

    ///e.g. "a.b.c" -> "a",Some("b.c") ; "a.b",Some("c") ; "a.b.c",None
    member this.Expansion =
        seq {
            let lastIndex = parts.Length-1
            for i in 0..lastIndex do
                let remaining =
                    if i = lastIndex then 
                        None 
                    else 
                        Some(Path(parts.[i+1..lastIndex]))
                
                yield Path(parts.[0..i]), remaining
        }

    override this.Equals(other:obj) =
        match other with
        | :? Path as other -> 
            parts = (other.Parts |> Seq.toArray)
        | _ -> false

    override this.GetHashCode() =
        this.Text.GetHashCode()
        

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
    ///get the value of a path (var, field, property, ...)
    | PathGet          of Path
    ///set the value of a path (var, field, property, ...)
    | PathSet          of Path * (SynExpr * PositionRange)
    ///call instance method on a variable or call a static method or call a constructor
    | PathCall         of Path * TySig list * SynExpr list * PositionRange
    ///static type name * static type generic args * method name * (optional) method generic args * method args * position
    | GenericTypeStaticCall of string * TySig list * string * TySig list * SynExpr list * PositionRange
    ///call instance method on an expression
    ///instance expresion * instance method name * (optional) generic type args * method arguments * pos info
    | ExprPathCall          of SynExpr * Path * TySig list * SynExpr list * PositionRange
    | ExprPathGet           of SynExpr * Path
    | ExprPathSet           of SynExpr * Path * (SynExpr * PositionRange)
    //| ExprPathGet           of SynExpr * Path * TySig list * SynExpr list * PositionRange
    //| ExprDataMember    of SynExpr * (Path * PositionRange)
    ///discard left hand side, return right hand side
    | Sequential       of SynExpr * (SynExpr * PositionRange)
    ///open a namespace or type
    | OpenNamespaceOrType of TySig * SynExpr 
    ///reference an assembly by name or dll path
    | OpenAssembly     of (string * PositionRange) * SynExpr
    | LogicalNot       of SynExpr * PositionRange
    | Cast             of SynExpr * TySig * PositionRange
    | IfThenElse       of (SynExpr * PositionRange) * SynExpr * SynExpr option * PositionRange //should be pos for each!
    | ComparisonBinop  of SynComparisonBinop * SynExpr * SynExpr * PositionRange
    | Nop
    | WhileLoop        of (SynExpr * PositionRange) * SynExpr
    | Break            of PositionRange
    | Continue         of PositionRange
    | LogicBinop       of SynLogicBinop * (SynExpr * PositionRange) * (SynExpr * PositionRange)
    | Checked          of SynExpr
    | Unchecked        of SynExpr

type SynStmt =
    | Let                   of string * (SynExpr * PositionRange)
    | OpenNamespaceOrType   of TySig
    | OpenAssembly          of string * PositionRange
    | Do                    of SynExpr

//represents a "raw" top level language element
type SynTopLevel =
    | Expr          of SynExpr
    | StmtList      of SynStmt list

