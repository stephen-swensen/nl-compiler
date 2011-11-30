namespace Swensen.NL

open System

///For semantic analysis, we enumerate each case instead of making LtEq, GtEq, and Neq merely syntactic compound forms.
type tcomparisonBinop = Eq | Lt | Gt
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
type texp =
    | Double        of float
    | Int32         of int
    | String        of string
    | Char          of char
    | Bool          of bool
    | Null          of Type
    | Typeof        of Type
    | Var           of string * Type
    | Default       of Type
    | Nop
    | Break         
    | Continue
    | Error         of Type

    | NumericBinop  of Ast.SynNumericBinop * texp * texp * Type
    | UMinus        of texp * Type
    | Let           of string * texp * texp * Type
    //primitive coersion
    | Coerce        of texp * Type
    ///box / box value type or down / up cast ref type
    | Cast          of texp * Type
    | StaticCall    of System.Reflection.MethodInfo * texp list * Type
    | InstanceCall  of texp * System.Reflection.MethodInfo * texp list * Type
    | Sequential    of texp * texp * Type
    | Ctor          of System.Reflection.ConstructorInfo * texp list * Type
    ///Default value of ValueType ("zero") or Ref type (null)
    | LogicalNot    of texp
    | IfThen        of texp * texp
    | IfThenElse    of texp * texp * texp * Type
    | ComparisonBinop  of tcomparisonBinop * texp * texp
    | VarSet        of string * texp
    | WhileLoop     of texp * texp
//    | Xor           of texp * texp
    with 
        member this.Type =
            match this with
            | LogicalNot _           -> typeof<bool>
            | Double(_)              -> typeof<float>
            | Int32(_)               -> typeof<int>
            | String(_)              -> typeof<string>
            | Char(_)                -> typeof<char>
            | Bool(_)                -> typeof<bool>
            | Typeof(_)              -> typeof<Type>
            | IfThen(_,_)            -> typeof<Void>
            | ComparisonBinop(_,_,_) -> typeof<bool>
            | Nop                    -> typeof<Void>
            | VarSet(_,_)            -> typeof<Void>
            | Break                  -> typeof<Void>
            | Continue               -> typeof<Void>
            | WhileLoop(_,_)         -> typeof<Void>
            //| LogicBinop _           -> typeof<bool>
            //| Xor(_,_)               -> typeof<bool>
        //    | NliReturn _            -> typeof<obj[]>
            | NumericBinop(_,_,_,ty)
            | UMinus(_,ty)
            | Let(_,_,_,ty)
            | Var(_,ty) 
            | Coerce(_,ty)
            | Cast(_,ty)
            | StaticCall(_,_,ty)
            | InstanceCall(_,_,_,ty) 
            | Sequential(_,_,ty)
            | Ctor(_,_,ty)
            | Default(ty)
            | Null(ty)
            | IfThenElse(_,_,_,ty)
            //ty is the recovery typeof the expression. (maybe add optional first parameter with the specific branch which was type checked but in error?)
            | Error(ty)
                -> ty
        
        ///make a comparison binop case using a Ast.SynComparisonBinop
        static member mkComparisonBinop(op:Ast.SynComparisonBinop, x:texp, y:texp) = 
            match op with
            | Ast.SynComparisonBinop.Eq -> texp.ComparisonBinop(tcomparisonBinop.Eq,x,y)
            | Ast.SynComparisonBinop.Lt -> texp.ComparisonBinop(tcomparisonBinop.Lt,x,y)
            | Ast.SynComparisonBinop.Gt -> texp.ComparisonBinop(tcomparisonBinop.Gt,x,y)
            | Ast.SynComparisonBinop.Neq -> 
                texp.LogicalNot(ComparisonBinop(tcomparisonBinop.Eq,x,y))
            | Ast.SynComparisonBinop.LtEq ->
                texp.LogicalNot(ComparisonBinop(tcomparisonBinop.Gt,x,y))
            | Ast.SynComparisonBinop.GtEq ->
                texp.LogicalNot(ComparisonBinop(tcomparisonBinop.Lt,x,y))