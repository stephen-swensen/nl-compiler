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
    | SByte             of SByte //y
    | Byte              of Byte //uy

    | Int16             of Int16 //s
    | UInt16            of UInt16 //us
    
    | Int32             of Int32 //no suffix
    | UInt32            of UInt32 //u
    
    | Int64             of Int64 //L
    | UInt64            of UInt64 //UL
    
    | Single            of Single //f
    | Double            of Double //no suffix
        
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
    //bool indicates whether checked
    | NumericBinop  of bool * ILNumericBinop * ILExpr * ILExpr * Type
    //bool indicates whether checked
    | UMinus        of bool * ILExpr * Type
    | Let           of string * ILExpr * ILExpr * Type
    //primitive coersion
    //bool indicates whether checked
    | Coerce        of bool * ILExpr * Type
    ///box / box value type or down / up cast ref type
    | Cast          of ILExpr * Type
    | StaticCall    of System.Reflection.MethodInfo * ILExpr list
    | InstanceCall  of ILExpr * System.Reflection.MethodInfo * ILExpr list
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
            | SByte _               -> typeof<SByte> //y
            | Byte  _               -> typeof<Byte> //uy

            | Int16 _               -> typeof<Int32> //s
            | UInt16 _              -> typeof<UInt32> //us
    
            | Int32 _               -> typeof<Int32> //no suffix
            | UInt32 _              -> typeof<UInt32> //u
    
            | Int64 _               -> typeof<Int64> //L
            | UInt64 _              -> typeof<UInt64> //UL
    
            | Single _              -> typeof<Single> //f
            | Double _              -> typeof<Double> //no suffix

            | String _              -> typeof<string>
            | Char _                -> typeof<char>
            | Bool _                -> typeof<bool>

            | LogicalNot _          -> typeof<bool>

            | Typeof _              -> typeof<Type>
            | ComparisonBinop _     -> typeof<bool>
            | StaticFieldGet fi     -> fi.FieldType
            | InstanceFieldGet(_,fi) -> fi.FieldType
            | StaticCall(mi,_)       -> mi.ReturnType
            | InstanceCall(_,mi,_)   -> mi.ReturnType

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
            | NumericBinop(_,_,_,_,ty)
            | UMinus(_,_,ty)
            | Let(_,_,_,ty)
            | VarGet(_,ty) 
            | Coerce(_,_,ty)
            | Cast(_,ty)
            | Sequential(_,_,ty)
            | Ctor(_,_,ty)
            | Default(ty)
            | Null(ty)
            | IfThenElse(_,_,_,ty)
            | Error(ty) //ty is the recovery typeof the expression. (maybe add optional first parameter with the specific branch which was type checked but in error?)
                -> ty

        ///maybe the following abstractions don't belong here...
        
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
        static member mkNumericBinop(cked:bool, op:Ast.SynNumericBinop, x:ILExpr, y:ILExpr, ty:Type) = 
            let op =
                match op with
                | Ast.SynNumericBinop.Div -> ILNumericBinop.Div
                | Ast.SynNumericBinop.Plus -> ILNumericBinop.Plus
                | Ast.SynNumericBinop.Times -> ILNumericBinop.Times
                | Ast.SynNumericBinop.Minus -> ILNumericBinop.Minus

            ILExpr.NumericBinop(cked, op, x, y, ty)

        static member InstancePropertySet(instance:ILExpr, pi:PropertyInfo, assign:ILExpr) =
            ILExpr.InstanceCall(instance, pi.GetSetMethod(), [assign])

        static member InstancePropertyGet(instance:ILExpr, pi:PropertyInfo) =
            ILExpr.InstanceCall(instance, pi.GetGetMethod(), [])

        static member StaticPropertySet(pi:PropertyInfo, assign:ILExpr) =
            ILExpr.StaticCall(pi.GetSetMethod(), [assign])

        static member StaticPropertyGet(pi:PropertyInfo) =
            ILExpr.StaticCall(pi.GetGetMethod(), [])

        static member mkStaticFieldGet(fi:FieldInfo) =
            if fi.IsLiteral && not fi.IsInitOnly then
                let fiTy = 
                    let fiTy = fi.FieldType
                    if fiTy.IsEnum then
                        fiTy.GetEnumUnderlyingType()
                    else
                        fiTy

                let fiVal = fi.GetValue(null)

                if not fiTy.IsValueType && fiTy <> typeof<string> then
                    Null(fiTy)
                elif fiTy = typeof<String> then
                    String(fiVal :?> String)
                elif fiTy = typeof<Int32> then
                    Int32(fiVal :?> Int32)
                elif fiTy = typeof<Boolean> then
                    Bool(fiVal :?> Boolean)
                elif fiTy = typeof<Double> then
                    Double(fiVal :?> Double)
                else
                    failwithf "const field of type '%s' with value '%s' not currently supported" fiTy.Name (fiVal |> string)
            else
                StaticFieldGet(fi)

        override this.ToString() =
            sprintf "%A" this

///represents a top level statement
type ILStmt =
    //variable stmt
    | Let               of string * ILExpr
    //expression stmt
    | Do                of ILExpr

///represents a semantically checked top level language element
type ILTopLevel =
    | Expr               of ILExpr
    | StmtList           of ILStmt list
    | Error

    ///Try to "normalize" the top level NL fragment to a list of statements
    member this.NormalizedStmts = 
        match this with
        | ILTopLevel.StmtList(stmts) -> Some(stmts)
        | ILTopLevel.Expr(x) -> Some([ILStmt.Do(x)])
        | ILTopLevel.Error -> None

    ///Try to "normalize" the top level NL fragment to a single expression
    member this.NormalizedExpr =
        match this with
        | ILTopLevel.Expr(x) -> Some(x)
        | ILTopLevel.StmtList([ILStmt.Do(x)]) -> Some(x)
        | _ -> None