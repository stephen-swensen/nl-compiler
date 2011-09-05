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

let coerceIfNeeded expectedTy (targetExp:texp) =
    if targetExp.Type <> expectedTy then Coerce(targetExp,expectedTy) else targetExp

let castIfNeeded expectedTy (targetExp:texp) =
    if targetExp.Type <> expectedTy then Cast(targetExp,expectedTy) else targetExp

let castEachIfNeeded expectedTys targetExps =
    List.zip expectedTys targetExps
    |> List.map (fun (targetTy, targetExp) -> castIfNeeded targetTy targetExp)

let castArgsIfNeeded (expectedParameters:ParameterInfo[]) targetExps =
    List.zip (expectedParameters |> Seq.map (fun p -> p.ParameterType) |> Seq.toList)  targetExps
    |> List.map (fun (targetTy, targetExp) -> castIfNeeded targetTy targetExp)

///Symantic analysis (type checking)
let rec tycheck refAsms openNames varEnv rawExpression =
    ///try to resolve the given type in the refAsms and openNames context; return null if fail to resolve
    let rec resolveType gsig =
        let name,args =
            match gsig with
            | Generic(name, args) -> name,args
            | NonGeneric(name) -> name,[]

        if name = "" then null
        else
            seq {
                for possibleName in (name::(openNames |> List.map (fun n -> n + "." + name))) do
                    for possibleAsm in refAsms do
                        if args = [] then
                            let possibleFullName = possibleName + ", " + possibleAsm
                            let ty = Type.GetType(possibleFullName,false, true)
                            yield ty
                        else
                            let argTys = List.map resolveType args
                            if List.exists ((=)null) argTys then
                                yield null
                            else
                                let possibleFullName = 
                                    sprintf "%s`%i[%s]" 
                                        possibleName 
                                        args.Length 
                                        (String.concat "," (List.map (fun (argTy:Type) -> sprintf "[%s]" argTy.AssemblyQualifiedName) argTys))
                                let ty = Type.GetType(possibleFullName,false, true)
                                yield ty
            } |> Seq.tryFind (fun ty -> ty <> null) |> function Some(ty) -> ty | None -> null

    match rawExpression with
    | rexp.Double x -> texp.Double x
    | rexp.Int32 x  -> texp.Int32 x
    | rexp.String x -> texp.String x
    | rexp.Char x   -> texp.Char x
    | rexp.Bool x   -> texp.Bool x
    | rexp.Null(name, pos)   -> 
        let ty = resolveType name
        checkNull ty (fun () -> semError pos (sprintf "could not resolve type of null: %A" name))
        if ty.IsValueType then
            semError pos (sprintf "null is not a valid value for the value type %s" ty.Name)
        texp.Null(ty)
    | rexp.Typeof(name, pos)   -> 
        let ty = resolveType name
        checkNull ty (fun () -> semError pos (sprintf "could not resolve type in type literal expression: %A" name))
        texp.Typeof(ty)
    | rexp.UMinus(x,pos) ->
        let x = tycheck refAsms openNames varEnv x
        texp.UMinus(x,x.Type)
    | rexp.Fact(x,pos) ->
        let x = tycheck refAsms openNames varEnv x
        if x.Type <> typeof<int> then
            semError pos (sprintf "factorial expects int but got: %A" x.Type)
        else
            let meth = typeof<CoreOps>.GetMethod("Factorial",[|typeof<int>|])
            texp.StaticCall(meth, [x], meth.ReturnType)
    | rexp.Pow(x,y,pos) ->
        let x, y = tycheck refAsms openNames varEnv x, tycheck refAsms openNames varEnv y
        let meth = typeof<System.Math>.GetMethod("Pow",[|typeof<float>;typeof<float>|])
        checkMeth meth pos "Pow" [x;y]
        texp.StaticCall(meth, [x;y] |> List.map (coerceIfNeeded typeof<float>) , meth.ReturnType)
    | rexp.NumericBinop(op,x,y,pos) ->
        let x, y = tycheck refAsms openNames varEnv x, tycheck refAsms openNames varEnv y
        let retTy =
            if x.Type = typeof<float> || y.Type = typeof<float> then
                Some(typeof<float>)
            elif x.Type = typeof<int> && y.Type = typeof<int> then
                Some(typeof<int>)
            else
                None
        match retTy with
        | Some(retTy) -> //primitive
            texp.NumericBinop(op, coerceIfNeeded retTy x, coerceIfNeeded retTy y, retTy)
        | None when op = Plus && (x.Type = typeof<string> || y.Type = typeof<string>) -> //string concat
            let meth = typeof<System.String>.GetMethod("Concat",[|x.Type; y.Type|])
            checkMeth meth pos "Concat" [x;y]
            texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)
        | None -> //static "op_*" overloads
            let opName =
                match op with
                | Plus -> "op_Addition"
                | Minus -> "op_Subtraction"
                | Div -> "op_Division"
                | Times -> "op_Multiply"
            let meth = 
                match x.Type.GetMethod(opName, [|x.Type; y.Type|]) with
                | null -> y.Type.GetMethod(opName, [|x.Type; y.Type|])
                | meth -> meth
            if meth = null then
                semError pos (sprintf "No overloads found for binary operator %A with left-hand-side type %A and right-hand-side type %A" op x.Type y.Type)
            else
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)    
    | rexp.NameCall(longName, genericArgs, args, pos) ->
        let namePrefix, methodName =
            let split = longName.Split('.')
            String.Join(".",split.[..split.Length-2]), split.[split.Length-1]
        let args = args |> List.map (tycheck refAsms openNames varEnv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        match Map.tryFind namePrefix varEnv with //N.B. vars always supercede open names
        | Some(instanceTy:Type) -> 
            let meth = instanceTy.GetMethod(methodName, instanceFlags, null, argTys, null)
            checkNull meth (fun () -> semError pos (sprintf "not a valid instance method: %s, for the given instance type: %s, and arg types: %A" methodName instanceTy.Name argTys))
            texp.InstanceCall(Var(namePrefix,instanceTy), meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | None ->
            let withGenericArgs name args =
                match args with
                | Some(args) -> Generic(name, args)
                | None -> NonGeneric(name)

            let ty = resolveType (withGenericArgs namePrefix genericArgs)
            if ty <> null then
                let meth = ty.GetMethod(methodName, staticFlags, null, argTys, null)
                checkNull meth (fun () -> semError pos (sprintf "not a valid static method: %s, for the given class type: %s, and arg types: %A" methodName namePrefix argTys))
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
            else        
                let ty = resolveType (withGenericArgs longName genericArgs)
                checkNull ty (fun () -> semError pos (sprintf "could not resolve method call type: %s or constructor type: %s" namePrefix longName))
                let ctor = ty.GetConstructor(argTys)
                checkNull ctor (fun () -> semError pos (sprintf "could not resolve constructor for type: %s with arg types: %A" ty.Name (args |> List.map(fun arg -> arg.Type))))
                texp.Ctor(ctor, castArgsIfNeeded (ctor.GetParameters()) args, ty)
                
    | rexp.ExpCall(instance,methodName, args, pos) ->
        let instance = tycheck refAsms openNames varEnv instance
        let args = args |> List.map (tycheck refAsms openNames varEnv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        let meth = instance.Type.GetMethod(methodName, instanceFlags, null, argTys, null)
        checkNull meth (fun () -> semError pos (sprintf "not a valid instance method: %s, for the given instance type: %s, and arg types: %A" methodName  instance.Type.Name argTys))
        InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
    | rexp.Let(name, assign, body, pos) ->
        let assign = tycheck refAsms openNames varEnv assign
        let body = tycheck refAsms openNames (varEnv |> Map.add name assign.Type) body
        texp.Let(name,assign, body, body.Type)
    | rexp.Var(name, pos) ->
        match Map.tryFind name varEnv with
        | Some(ty) -> texp.Var(name,ty)
        | None -> semError pos (sprintf "Var not found in environment: %s" name)
    | rexp.Sequential(x,y, pos) ->
        let x, y = tycheck refAsms openNames varEnv x, tycheck refAsms openNames varEnv y
        texp.Sequential(x,y,y.Type)
    | rexp.Open(name, x, _) ->
        tycheck refAsms (name::openNames) varEnv x
    | rexp.Ref(name, x, pos) ->
        let name =
            try
                Assembly.Load(name) |> ignore
                name
            with _ ->
                try 
                    //are we sure we don't want to use LoadFile so that we can use reference in different scopes?
                    Assembly.LoadFrom(name).FullName
                with _ ->
                    semError pos (sprintf "Unable to resolve assembly reference: %s" name)
        tycheck (name::refAsms) openNames varEnv x