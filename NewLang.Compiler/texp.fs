namespace Swensen.NewLang

open System

///Typed expression
type texp =
    | Double        of float
    | Int32         of int
    | String        of string
    | Char          of char
    | Bool          of bool
    | Null          of Type
    | Typeof        of Type
    | NumericBinop  of numericBinop * texp * texp * Type
    | UMinus        of texp * Type
    | Let           of string * texp * texp * Type
    | Var           of string * Type
    //primitive coersion
    | Coerce        of texp * Type
    //downcast or box type
    | Cast          of texp * Type
    | StaticCall    of System.Reflection.MethodInfo * texp list * Type
    | InstanceCall  of texp * System.Reflection.MethodInfo * texp list * Type
    | Sequential    of texp * texp * Type
    | Ctor          of System.Reflection.ConstructorInfo * texp list * Type
    //initialize a value type ("zeroed-out" fields)
    | DefaultCtor  of Type
    | Not          of texp * Type
    with 
        member this.Type =
            match this with
            | Double(_)              -> typeof<float>
            | Int32(_)               -> typeof<int>
            | String(_)              -> typeof<string>
            | Char(_)                -> typeof<char>
            | Bool(_)                -> typeof<bool>
            | Typeof(_)              -> typeof<Type>
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
            | DefaultCtor(ty)
            | Null(ty)
            | Not(_,ty)
                -> ty