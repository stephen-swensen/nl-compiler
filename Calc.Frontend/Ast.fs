module Swensen.Calc.Ast
open System

type binop =
    | Plus | Minus | Times | Div | Pow

///"UnTyped" expressions produces by the parser before typed checking
module UT =
    type exp =
        | Double    of float
        | Int32     of int
        | Binop     of binop * exp * exp
        | UMinus    of exp
        | Fact      of exp
        | Let       of string * exp * exp
        | Var       of string
        //| Call      of string *  exp list

///Symantically check expressions generated from UT.exp
type exp =
    | Double    of float * Type
    | Int32     of int * Type
    | Binop     of binop * exp * exp * Type
    | UMinus    of exp * Type
    | Fact      of exp * Type
    | Let       of string * exp * exp * Type
    | Var       of string * Type
    | Coerce    of exp * Type
    //| Call      of string * exp list * Type

    with 
        member this.Type =
            match this with
            | Double(_,ty)
            | Int32(_,ty)
            | Binop(_,_,_,ty)
            | UMinus(_,ty)
            | Fact(_,ty) 
            | Let(_,_,_,ty)
            | Var(_,ty) 
            | Coerce(_,ty)
            //| Call(_,_,ty)
                -> ty

///Symantic analysis (type checking)
let rec tycheck venv exp =
    match exp with
    | UT.Double(x) -> Double(x,typeof<float>)
    | UT.Int32(x) -> Int32(x,typeof<int>)
    | UT.UMinus(x) ->
        let x = tycheck venv x
        UMinus(x,x.Type)
    | UT.Fact(x) ->
        let x = tycheck venv x
        if x.Type <> typeof<int> then
            failwithf "factorial expects int but got: %A" x.Type
        else
            Fact(x, x.Type)
    | UT.Binop(op,x,y) ->
        let x, y = tycheck venv x, tycheck venv y
        let ty =
            if op = Pow || x.Type = typeof<float> || y.Type = typeof<float> then
                typeof<float>
            elif x.Type = typeof<int> && y.Type = typeof<int> then
                typeof<int>
            else
                failwithf "numeric binop expects float or int args but got lhs=%A, rhs=%A" x.Type y.Type 
        Binop(op,(if x.Type <> ty then Coerce(x,ty) else x),(if y.Type <> ty then Coerce(y,ty) else y),ty)
//    | UT.Call(name, exps) =
//        let expTys = exps |> List.map tycheck
//        let meth = Type.Get
    | UT.Let(id, assign, body) ->
        let assign = tycheck venv assign
        let body = tycheck (venv |> Map.add id assign.Type) body
        Let(id,assign, body, body.Type)
    | UT.Var(id) ->
        match Map.tryFind id venv with
        | Some(ty) -> Var(id,ty)
        | None -> failwithf "Var not found in environment: %s" id