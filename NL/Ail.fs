///Abstract IL datatypes
module Swensen.NL.Ail

open System
open System.Reflection

type ILNumericBinop = Plus | Minus | Times | Div
    with
        ///Call the F# analog to this operator on the operands (used for constants folding during optimization)
        member inline x.Call(lhs:'a,rhs:'a):'a =
            let fsop =
                match x with
                | Plus -> (+)
                | Div -> (/)
                | Minus -> (-)
                | Times -> (*)

            fsop lhs rhs

///For semantic analysis, we enumerate each case instead of making LtEq, GtEq, and Neq merely syntactic compound forms.
type ILComparisonBinop = Eq | Lt | Gt
    with
        ///Call the F# analog to this operator on the operands
        member inline x.Call(lhs:'a,rhs:'a):bool =
            let fsop =
                match x with
                | Eq -> (=)
                | Lt -> (<)
                | Gt -> (>)

            fsop lhs rhs

///Typed expression
type ILExpr =
    | Double        of float
    | Int32         of int
    | String        of string
    | Char          of char
    | Bool          of bool
    | Null          of Type
    | Typeof        of Type
    | VarGet        of string * Type
    //Default value of ValueType ("zero") or Ref type (null)
    | Default       of Type
    | Nop
    | Break         
    | Continue
    | Error         of Type

    | NumericBinop  of ILNumericBinop * ILExpr * ILExpr * Type
    | UMinus        of ILExpr * Type
    | Let           of string * ILExpr * ILExpr * Type
    //primitive coersion
    | Coerce        of ILExpr * Type
    ///box / box value type or down / up cast ref type
    | Cast          of ILExpr * Type
    | StaticCall    of System.Reflection.MethodInfo * ILExpr list * Type
    | InstanceCall  of ILExpr * System.Reflection.MethodInfo * ILExpr list * Type
    | Sequential    of ILExpr * ILExpr * Type
    | Ctor          of System.Reflection.ConstructorInfo * ILExpr list * Type
    | LogicalNot    of ILExpr
    | IfThen        of ILExpr * ILExpr
    | IfThenElse    of ILExpr * ILExpr * ILExpr * Type
    | ComparisonBinop  of ILComparisonBinop * ILExpr * ILExpr
    | VarSet        of string * ILExpr
    | WhileLoop     of ILExpr * ILExpr
    | StaticFieldSet      of FieldInfo * ILExpr
    | StaticFieldGet      of FieldInfo
    | InstanceFieldGet    of ILExpr * FieldInfo
    | InstanceFieldSet    of ILExpr * FieldInfo * ILExpr
    with 
        member this.Type =
            match this with
            //Other implicit
            | LogicalNot _          -> typeof<bool>
            | Double _              -> typeof<float>
            | Int32 _               -> typeof<int>
            | String _              -> typeof<string>
            | Char _                -> typeof<char>
            | Bool _                -> typeof<bool>
            | Typeof _              -> typeof<Type>
            | ComparisonBinop _     -> typeof<bool>
            | StaticFieldGet fi     -> fi.FieldType
            | InstanceFieldGet(_,fi) -> fi.FieldType
            //Always void
            | IfThen _
            | Nop
            | VarSet _
            | Break
            | Continue
            | StaticFieldSet _
            | InstanceFieldSet _
            | WhileLoop _           -> typeof<Void>

            //Explicitly constructed with types
            | NumericBinop(_,_,_,ty)
            | UMinus(_,ty)
            | Let(_,_,_,ty)
            | VarGet(_,ty) 
            | Coerce(_,ty)
            | Cast(_,ty)
            | StaticCall(_,_,ty)
            | InstanceCall(_,_,_,ty) 
            | Sequential(_,_,ty)
            | Ctor(_,_,ty)
            | Default(ty)
            | Null(ty)
            | IfThenElse(_,_,_,ty)
            | Error(ty) //ty is the recovery typeof the expression. (maybe add optional first parameter with the specific branch which was type checked but in error?)
                -> ty
        
        ///make a comparison binop case using an Ast.SynComparisonBinop
        static member mkComparisonBinop(op:Ast.SynComparisonBinop, x:ILExpr, y:ILExpr) = 
            match op with
            | Ast.SynComparisonBinop.Eq -> ILExpr.ComparisonBinop(ILComparisonBinop.Eq,x,y)
            | Ast.SynComparisonBinop.Lt -> ILExpr.ComparisonBinop(ILComparisonBinop.Lt,x,y)
            | Ast.SynComparisonBinop.Gt -> ILExpr.ComparisonBinop(ILComparisonBinop.Gt,x,y)
            | Ast.SynComparisonBinop.Neq -> 
                ILExpr.LogicalNot(ILExpr.ComparisonBinop(ILComparisonBinop.Eq,x,y))
            | Ast.SynComparisonBinop.LtEq ->
                ILExpr.LogicalNot(ILExpr.ComparisonBinop(ILComparisonBinop.Gt,x,y))
            | Ast.SynComparisonBinop.GtEq ->
                ILExpr.LogicalNot(ILExpr.ComparisonBinop(ILComparisonBinop.Lt,x,y))

        ///make a comparison binop case using an Ast.SynNumericBinop
        static member mkNumericBinop(op:Ast.SynNumericBinop, x:ILExpr, y:ILExpr, ty:Type) = 
            let op =
                match op with
                | Ast.SynNumericBinop.Div -> ILNumericBinop.Div
                | Ast.SynNumericBinop.Plus -> ILNumericBinop.Plus
                | Ast.SynNumericBinop.Times -> ILNumericBinop.Times
                | Ast.SynNumericBinop.Minus -> ILNumericBinop.Minus

            ILExpr.NumericBinop(op, x, y, ty)

///represents a top level statement
type ILStmt =
    //variable stmt
    | Let               of string * ILExpr
    //expression stmt
    | Do                of ILExpr

///represents a semantically checked top level language element
type ILTopLevel =
    | Exp               of ILExpr
    | StmtList          of ILStmt list
    | Error