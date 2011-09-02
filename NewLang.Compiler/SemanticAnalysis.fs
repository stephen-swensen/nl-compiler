module Swensen.NewLang.SemanticAnalysis

open System
open System.Reflection
open Swensen.NewLang

let semError pos msg = raise <| SemanticErrorException(pos, msg)
let checkNull x f = if x = null then f()

///Binding flags for our language
let instanceFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.IgnoreCase
let staticFlags = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.IgnoreCase

let checkMeth (meth:MethodInfo) pos name tys =
    checkNull meth (fun () -> semError pos (sprintf "method %s not found for parameter types %A" name tys))

let coerceIfNeeded  (expectedTy:Type) (targetExp:texp) =
    if targetExp.Type <> expectedTy then Coerce(targetExp,expectedTy) else targetExp

///Symantic analysis (type checking)
let rec tycheck openNames venv rawExpression =
    match rawExpression with
    | rexp.Double x -> texp.Double x
    | rexp.Int32 x  -> texp.Int32 x
    | rexp.String x -> texp.String x
    | rexp.Char x   -> texp.Char x
    | rexp.UMinus(x,pos) ->
        let x = tycheck openNames venv x
        texp.UMinus(x,x.Type)
    | rexp.Fact(x,pos) ->
        let x = tycheck openNames venv x
        if x.Type <> typeof<int> then
            semError pos (sprintf "factorial expects int but got: %A" x.Type)
        else
            let meth = typeof<CoreOps>.GetMethod("Factorial",[|typeof<int>|])
            texp.StaticCall(meth, [x], meth.ReturnType)
    | rexp.Concat(x,y,pos) ->
        let x, y = tycheck openNames venv x, tycheck openNames venv y
        if x.Type = typeof<string> || y.Type = typeof<string> then
            let meth = typeof<System.String>.GetMethod("Concat",[|x.Type; y.Type|])
            checkMeth meth pos "Concat" [x;y]
            texp.StaticCall(meth, [x;y], meth.ReturnType)
        else
            semError pos (sprintf "invalid types for Concat: %A" [x;y])
    | rexp.Pow(x,y,pos) ->
        let x, y = tycheck openNames venv x, tycheck openNames venv y
        let meth = typeof<System.Math>.GetMethod("Pow",[|typeof<float>;typeof<float>|])
        checkMeth meth pos "Pow" [x;y]
        texp.StaticCall(meth, [x;y] |> List.map (coerceIfNeeded typeof<float>) , meth.ReturnType)
    | rexp.NumericBinop(op,x,y,pos) ->
        let x, y = tycheck openNames venv x, tycheck openNames venv y
        let ty =
            if x.Type = typeof<float> || y.Type = typeof<float> then
                typeof<float>
            elif x.Type = typeof<int> && y.Type = typeof<int> then
                typeof<int>
            else
                semError pos (sprintf "numeric binop expects float or int args but got lhs=%A, rhs=%A" x.Type y.Type)
        texp.NumericBinop(op, coerceIfNeeded ty x, coerceIfNeeded ty y, ty)
    | rexp.NameCall(longName, args, pos) ->
        let namePrefix, methodName =
            let split = longName.Split('.')
            String.Join(".",split.[..split.Length-2]), split.[split.Length-1]
        let args = args |> List.map (tycheck openNames venv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        match Map.tryFind namePrefix venv with
        | Some(instanceTy:Type) -> 
            let meth = instanceTy.GetMethod(methodName, instanceFlags, null, argTys, null)
            checkNull meth (fun () -> semError pos (sprintf "not a valid method: %s, for the given instance type: %s, and arg types: %A" methodName instanceTy.Name argTys))
            texp.InstanceCall(Var(namePrefix,instanceTy), meth, args, meth.ReturnType)
        | None ->
            let ty = Type.GetType(namePrefix,false,true)
            if ty = null then
                let ty = Type.GetType(longName,false,true)
                checkNull ty (fun () -> semError pos (sprintf "could not resolve method call type %s or constructor type %s" namePrefix longName))
                let ctor = ty.GetConstructor(argTys)
                texp.Ctor(ctor, args, ty)
            else        
                let meth = ty.GetMethod(methodName, staticFlags, null, argTys, null)
                checkNull meth (fun () -> semError pos (sprintf "not a valid method: %s, for the given class type: %s, and arg types: %A" namePrefix methodName argTys))
                texp.StaticCall(meth, args, meth.ReturnType)
    | rexp.ExpCall(instance,methodName, args, pos) ->
        let instance = tycheck openNames venv instance
        let args = args |> List.map (tycheck openNames venv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        let meth = instance.Type.GetMethod(methodName, instanceFlags, null, argTys, null)
        checkNull meth (fun () -> semError pos (sprintf "not a valid method: %s, for the given instance type: %s, and arg types: %A" instance.Type.Name methodName argTys))
        InstanceCall(instance, meth, args, meth.ReturnType)
    | rexp.Let(name, assign, body, pos) ->
        let assign = tycheck openNames venv assign
        let body = tycheck openNames (venv |> Map.add name assign.Type) body
        texp.Let(name,assign, body, body.Type)
    | rexp.Var(name, pos) ->
        match Map.tryFind name venv with
        | Some(ty) -> texp.Var(name,ty)
        | None -> semError pos (sprintf "Var not found in environment: %s" name)
    | rexp.Sequential(x,y, pos) ->
        let x, y = tycheck openNames venv x, tycheck openNames venv y
        texp.Sequential(x,y,y.Type)
    | rexp.Open(name, x, _) ->
        tycheck (name::openNames) venv x