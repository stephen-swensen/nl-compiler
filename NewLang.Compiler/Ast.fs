module Swensen.NewLang.Ast
open System

type numericBinop = Plus | Minus | Times | Div
type logicBinop = And | Or | ExOr

///"UnTyped" expressions produces by the parser before typed checking
module UT =
    type exp =
        | Double           of float
        | Int32            of int
        | String           of string
        | NumericBinop     of numericBinop * exp * exp
        | Concat           of exp * exp
        | Pow              of exp * exp
        | UMinus           of exp
        | Fact             of exp
        | Let              of string * exp * exp
        | Var              of string
        | IdCall           of string * exp list
        | ExpCall          of exp * string * exp list

///Symantically check expressions generated from UT.exp
type exp =
    | Double        of float
    | Int32         of int
    | String        of string
    | NumericBinop  of numericBinop * exp * exp * Type
    | UMinus        of exp * Type
    | Let           of string * exp * exp * Type
    | Var           of string * Type
    | Coerce        of exp * Type
    | StaticCall    of System.Reflection.MethodInfo * exp list * Type
    | InstanceCall  of exp * System.Reflection.MethodInfo * exp list * Type

    with 
        member this.Type =
            match this with
            | Double(_)              -> typeof<float>
            | Int32(_)               -> typeof<int>
            | String(_)              -> typeof<string>
            | NumericBinop(_,_,_,ty)
            | UMinus(_,ty)
            | Let(_,_,_,ty)
            | Var(_,ty) 
            | Coerce(_,ty)
            | StaticCall(_,_,ty)
            | InstanceCall(_,_,_,ty) -> ty

type CoreOps =
    static member Factorial(n:int) =
        let rec fact n = 
            match n with 
            | 1 | 0 -> 1
            | n     -> n * fact (n-1)
        fact n

open System.Reflection

///Binding flags for our language
let instanceFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.IgnoreCase
let staticFlags = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.IgnoreCase

let checkMeth (meth:MethodInfo) name tys =
    if meth = null then failwithf "method %s not found for parameter types %A" name tys

let coerceIfNeeded  (expectedTy:Type) (targetExp:exp) =
    if targetExp.Type <> expectedTy then Coerce(targetExp,expectedTy) else targetExp

///Symantic analysis (type checking)
let rec tycheck venv exp =
    match exp with
    | UT.Double(x) -> Double x
    | UT.Int32(x)  -> Int32 x
    | UT.String(x) -> String x
    | UT.UMinus(x) ->
        let x = tycheck venv x
        UMinus(x,x.Type)
    | UT.Fact(x) ->
        let x = tycheck venv x
        if x.Type <> typeof<int> then
            failwithf "factorial expects int but got: %A" x.Type
        else
            let meth = typeof<CoreOps>.GetMethod("Factorial",[|typeof<int>|])
            StaticCall(meth, [x], meth.ReturnType)
    | UT.Concat(x,y) ->
        let x, y = tycheck venv x, tycheck venv y
        if x.Type = typeof<string> || y.Type = typeof<string> then
            let meth = typeof<System.String>.GetMethod("Concat",[|x.Type; y.Type|])
            checkMeth meth "Concat" [x;y]
            StaticCall(meth, [x;y], meth.ReturnType)
        else
            failwithf "invalid types for Concat: %A" [x;y]
    | UT.Pow(x,y) ->
        let x, y = tycheck venv x, tycheck venv y
        let meth = typeof<System.Math>.GetMethod("Pow",[|typeof<float>;typeof<float>|])
        checkMeth meth "Pow" [x;y]
        StaticCall(meth, [x;y] |> List.map (coerceIfNeeded typeof<float>) , meth.ReturnType)
    | UT.NumericBinop(op,x,y) ->
        let x, y = tycheck venv x, tycheck venv y
        let ty =
            if x.Type = typeof<float> || y.Type = typeof<float> then
                typeof<float>
            elif x.Type = typeof<int> && y.Type = typeof<int> then
                typeof<int>
            else
                failwithf "numeric binop expects float or int args but got lhs=%A, rhs=%A" x.Type y.Type 
        NumericBinop(op, coerceIfNeeded ty x, coerceIfNeeded ty y, ty)
    | UT.IdCall(longId, args) ->
        let idLead, methodName =
            let split = longId.Split('.')
            String.Join(".",split.[..split.Length-2]), split.[split.Length-1]
        let args = args |> List.map (tycheck venv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        match Map.tryFind idLead venv with
        | Some(instanceTy:Type) -> 
            let meth = instanceTy.GetMethod(methodName, instanceFlags, null, argTys, null)
            if meth = null then
                failwithf "not a valid method: %s, for the given instance type: %s, and arg types: %A" instanceTy.Name methodName argTys
        
            InstanceCall(Var(idLead,instanceTy), meth, args, meth.ReturnType)
        | None ->        
            let ty = Type.GetType(idLead,true,true)
            if ty = null then
                failwithf "not a valid type: %s" idLead
        
            let meth = ty.GetMethod(methodName, staticFlags, null, argTys, null)
            if meth = null then
                failwithf "not a valid method: %s, for the given class type: %s, and arg types: %A" idLead methodName argTys
        
            StaticCall(meth, args, meth.ReturnType)
    | UT.ExpCall(instance,methodName, args) ->
        let instance = tycheck venv instance
        let args = args |> List.map (tycheck venv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        let meth = instance.Type.GetMethod(methodName, instanceFlags, null, argTys, null)
        if meth = null then
            failwithf "not a valid method: %s, for the given instance type: %s, and arg types: %A" instance.Type.Name methodName argTys
        
        InstanceCall(instance, meth, args, meth.ReturnType)
    | UT.Let(idLead, assign, body) ->
        let assign = tycheck venv assign
        let body = tycheck (venv |> Map.add idLead assign.Type) body
        Let(idLead,assign, body, body.Type)
    | UT.Var(idLead) ->
        match Map.tryFind idLead venv with
        | Some(ty) -> Var(idLead,ty)
        | None -> failwithf "Var not found in environment: %s" idLead