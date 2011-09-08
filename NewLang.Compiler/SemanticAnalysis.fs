module Swensen.NewLang.SemanticAnalysis

open System
open System.Reflection
open Swensen.NewLang

module Option =
    let fromNullable nullable =
        match nullable with
        | null -> None
        | _ -> Some(nullable)

let semError pos msg = raise <| SemanticErrorException(pos, msg)
let checkNull x f = if x = null then f()
let checkMeth (meth:MethodInfo) pos name tys =
    checkNull meth (fun () -> semError pos (sprintf "method %s not found for parameter types %A" name tys))

///Binding flags for our language
let instanceFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.IgnoreCase
let staticFlags = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.IgnoreCase

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

//todo: infer generic type arguments from type parameters (reflection not friendly for this)
//todo: file bug: name should not need a type constraint
let tryResolveMethod (ty:Type) (name:string) bindingFlags (genericTyArgs:Type[] option) (argTys: Type[]) =
    //todo: sophisticated overload resolution used both in generic and non-generic methods; note that
    //currently using reflection default overload resolution for non-generic methods and no overload resolution for non-generic methods when
    //types don't match exactly, we don't like this since it is asymetric
    match genericTyArgs with
    | Some(genericTyArgs) -> 
        let possibleMeths =
            ty.GetMethods(bindingFlags)
            |> Seq.filter 
                (fun meth -> 
                    (meth.Name.ToLower() = name.ToLower()) && 
                    meth.IsGenericMethod &&
                    meth.GetGenericArguments().Length = genericTyArgs.Length &&
                    meth.GetParameters().Length = argTys.Length)
            |> Seq.map (fun meth -> meth.MakeGenericMethod(genericTyArgs))
            |> Seq.filter (fun meth -> (meth.GetParameters() |> Array.map (fun meth -> meth.ParameterType)) = argTys)
            |> Seq.toArray
        match possibleMeths.Length with
        | 1 -> Some(possibleMeths.[0])
        | _ -> None
    | None -> //todo: handle type inference
        match ty.GetMethod(name, bindingFlags, null, argTys, null) with
        | null -> None
        | meth -> Some(meth)

///Symantic analysis (type checking)
let rec tycheck refAsms openNames varEnv rawExpression =
    ///try to resolve the given type in the refAsms and openNames context; return null if fail to resolve
    let rec tryResolveType gsig =
        let name,args =
            match gsig with
            | Generic(name, args) -> name,args
            | NonGeneric(name) -> name,[]

        match name with
        | "" -> None
        | _ ->
            seq {
                for possibleName in (name::(openNames |> List.map (fun n -> n + "." + name))) do
                    for possibleAsm in refAsms do
                        if args = [] then
                            let possibleFullName = possibleName + ", " + possibleAsm
                            let ty = Type.GetType(possibleFullName,false, true)
                            yield Option.fromNullable ty
                        else
                            let argTys = List.map tryResolveType args
                            if List.exists ((=)None) argTys then
                                yield None //todo: would like to convey exactly which sub types failed to resolve
                            else
                                let possibleFullName = 
                                    sprintf "%s`%i[%s], %s" 
                                        possibleName 
                                        args.Length 
                                        (String.concat "," (List.map (fun (argTy:Type) -> sprintf "[%s]" argTy.AssemblyQualifiedName) (List.choose id argTys)))
                                        possibleAsm
                                let ty = Type.GetType(possibleFullName,false, true)
                                yield Option.fromNullable ty
            } |> Seq.tryPick id

    //todo: make cleaner, either wait to convert to reflection friendly arrays till last moment, or convert to arrays up front
    let tryResolveGenericArgTys genericArgs =        
        let genericArgTys = genericArgs |> Seq.map tryResolveType
        if Seq.exists ((=)None) genericArgTys then None
        else Some(genericArgTys |> Seq.choose id |> Seq.toArray)

    //todo: don't like passing in pos here too much
    let tryResolveMethodWithGenericArgs ty methodName bindingFlags genericArgs argTys pos =
        match genericArgs with
        | Some(genericArgs) -> 
            match tryResolveGenericArgTys genericArgs with
            | None -> semError pos (sprintf "could not resolve generic arg types: %A" genericArgs)
            | genericArgTys -> tryResolveMethod ty methodName bindingFlags genericArgTys argTys
        | None ->
            tryResolveMethod ty methodName bindingFlags None argTys

    match rawExpression with
    | rexp.Double x -> texp.Double x
    | rexp.Int32 x  -> texp.Int32 x
    | rexp.String x -> texp.String x
    | rexp.Char x   -> texp.Char x
    | rexp.Bool x   -> texp.Bool x
    | rexp.Null(name, pos)   -> 
        match tryResolveType name with
        | None -> semError pos (sprintf "could not resolve type of null: %A" name)
        | Some(ty) ->
            if ty.IsValueType then
                semError pos (sprintf "null is not a valid value for the value type %s" ty.Name)
            texp.Null(ty)
    | rexp.Typeof(name, pos)   -> 
        match tryResolveType name with
        | None -> semError pos (sprintf "could not resolve type in type literal expression: %A" name)
        | Some(ty) -> texp.Typeof(ty)
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
    | rexp.NameCall(longName, genericArgs, args, pos) -> //todo: need more position info for different tokens
        let namePrefix, methodName =
            let split = longName.Split('.')
            String.Join(".",split.[..split.Length-2]), split.[split.Length-1]
        let args = args |> List.map (tycheck refAsms openNames varEnv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray

        match Map.tryFind namePrefix varEnv with //N.B. vars always supercede open names
        | Some(ty:Type) -> //instance method call on variable
            match tryResolveMethodWithGenericArgs ty methodName instanceFlags genericArgs argTys pos with
            | None -> semError pos (sprintf "not a valid instance method: %s, for the given instance type: %s, and arg types: %A" methodName ty.Name argTys)
            | Some(meth) -> texp.InstanceCall(Var(namePrefix,ty), meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | None ->
            match tryResolveType (NonGeneric(namePrefix)) with
            | Some(ty) -> //static method call (possibly generic) on non-generic type (need to handle generic type in another parse case, i think)
                match  tryResolveMethodWithGenericArgs ty methodName staticFlags genericArgs argTys pos with
                | None -> semError pos (sprintf "not a valid static method: %s, for the given class type: %s, and arg types: %A" methodName ty.Name argTys)
                | Some(meth) -> texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
            | None -> //constructors
                let withGenericArgs name =
                    match genericArgs with
                    | Some(genericArgs) -> Generic(name, genericArgs)
                    | None -> NonGeneric(name)

                match tryResolveType (withGenericArgs longName) with
                | None -> semError pos (sprintf "could not resolve method call type: %s or constructor type: %s" namePrefix longName)
                | Some(ty) ->
                    if ty.IsValueType && args.Length = 0 then
                        ///these optimization maybe should go in the Compiler emit
                        if ty = typeof<int32> then
                            texp.Int32(Unchecked.defaultof<int32>)
                        elif ty = typeof<double> then
                            texp.Double(Unchecked.defaultof<double>)
                        elif ty = typeof<bool> then
                            texp.Bool(Unchecked.defaultof<bool>)
                        elif ty = typeof<char> then
                            texp.Char(Unchecked.defaultof<char>)
                        else
                            texp.DefaultCtor(ty)
                    else
                        match ty.GetConstructor(argTys) with
                        | null -> semError pos (sprintf "could not resolve constructor for type: %s with arg types: %A" ty.Name (args |> List.map(fun arg -> arg.Type)))
                        | ctor -> texp.Ctor(ctor, castArgsIfNeeded (ctor.GetParameters()) args, ty)
    | rexp.GenericTypeStaticCall(tyName, tyGenericArgs, methodName, methodGenericArgs, args, pos) -> //todo: need more position info for different tokens
        match tryResolveType (Generic(tyName, tyGenericArgs)) with
        | None -> semError pos (sprintf "could not resolve type: %s with generic arg types: %A" tyName tyGenericArgs)
        | Some(ty) ->
            let args = args |> List.map (tycheck refAsms openNames varEnv)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            match tryResolveMethodWithGenericArgs ty methodName staticFlags methodGenericArgs argTys pos with
            | None -> semError pos (sprintf "not a valid static method: %s, for the given class type: %s, and arg types: %A" methodName ty.Name argTys)
            | Some(meth) ->
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
    | rexp.ExpCall(instance, methodName, methodGenericArgs, args, pos) ->
        let instance = tycheck refAsms openNames varEnv instance
        let args = args |> List.map (tycheck refAsms openNames varEnv)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        match tryResolveMethodWithGenericArgs instance.Type methodName instanceFlags methodGenericArgs argTys pos with
        | None -> semError pos (sprintf "not a valid instace method: %s, for the given expression type: %s, and arg types: %A" methodName instance.Type.Name argTys)
        | Some(meth) ->
            texp.InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
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