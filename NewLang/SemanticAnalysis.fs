﻿module Swensen.NewLang.SemanticAnalysis

open System
open System.Reflection

let semError pos msg = 
    raise <| SemanticAnalysisException(pos, msg)

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
///(ty:Type) (name:string) bindingFlags (genericTyArgs:Type[]) (argTys: Type[]) -> MethodInfo option
let tryResolveMethod (ty:Type) (name:string) bindingFlags (genericTyArgs:Type[]) (argTys: Type[]) =
    //todo: sophisticated overload resolution used both in generic and non-generic methods; note that
    //currently using reflection default overload resolution for non-generic methods and no overload resolution for non-generic methods when
    //types don't match exactly, we don't like this since it is asymetric
    match genericTyArgs with
    | [||] -> //todo: handle type inference
        match ty.GetMethod(name, bindingFlags, null, argTys, null) with
        | null -> None
        | meth -> Some(meth)
    | genericTyArgs -> 
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


let tryResolveBinaryOp opName (xty:Type) (yty:Type) = 
    seq { yield xty.GetMethod(opName, [|xty;yty|])
          yield yty.GetMethod(opName, [|xty;yty|]) } |> Seq.tryFind ((<>)null)

let tryResolveOpImplicit, tryResolveOpExplicit =
    let tryResolveConversionOp name (onty:Type) fromty toty =
        onty.GetMethods(staticFlags)
        |> Array.tryFind 
            (fun (meth:MethodInfo) -> 
                meth.Name = name &&
                meth.ReturnType = toty &&
                (meth.GetParameters() |> Array.map (fun p -> p.ParameterType)) = [|fromty|])

    (fun onty fromty toto -> tryResolveConversionOp "op_Implicit" onty fromty toto), 
    (fun onty fromty toto -> tryResolveConversionOp "op_Explicit" onty fromty toto)

///Symantic analysis (type checking)
let rec tycheckWith env rawExpression = // isLoopBody (refAsms:Assembly list) openNames varEnv rawExpression =
    let tycheck = tycheckWith env
    ///try to resolve the given type in the refAsms and openNames context; return null if fail to resolve
    let rec tryResolveType gsig =
        match gsig with
        | TySig("",_) -> None
        | TySig(name,args) ->
            seq {
                for possibleName in (name::(env.Namespaces |> List.map (fun n -> n + "." + name))) do
                    for possibleAsm in env.Assemblies do
                        if args = [] then
                            let possibleFullName = possibleName + ", " + possibleAsm.FullName
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
                                        possibleAsm.FullName
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
        | [||] ->
            tryResolveMethod ty methodName bindingFlags [||] argTys
        | genericArgs -> 
            match tryResolveGenericArgTys genericArgs with
            | None -> semError pos (sprintf "could not resolve generic arg types: %A" genericArgs)
            | Some(genericArgTys) -> tryResolveMethod ty methodName bindingFlags genericArgTys argTys

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
        let x = tycheck x
        texp.UMinus(x,x.Type)
    | rexp.Fact(x,pos) ->
        let x = tycheck x
        if x.Type <> typeof<int> then
            semError pos (sprintf "factorial expects int but got: %A" x.Type)
        else
            let meth = typeof<CoreOps>.GetMethod("Factorial",[|typeof<int>|])
            texp.StaticCall(meth, [x], meth.ReturnType)
    | rexp.Pow(x,y,pos) ->
        let x, y = tycheck x, tycheck y
        let meth = typeof<System.Math>.GetMethod("Pow",[|typeof<float>;typeof<float>|])
        checkMeth meth pos "Pow" [x;y]
        texp.StaticCall(meth, [x;y] |> List.map (coerceIfNeeded typeof<float>) , meth.ReturnType)
    | rexp.NumericBinop(op,x,y,pos) ->
        let x, y = tycheck x, tycheck y
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
            
            let meth = seq {
                yield x.Type.GetMethod(opName, [|x.Type; y.Type|])
                yield y.Type.GetMethod(opName, [|x.Type; y.Type|]) } |> Seq.tryFind ((<>)null)

            match meth with
            | None ->
                semError pos (sprintf "No overloads found for binary operator %A with left-hand-side type %A and right-hand-side type %A" op x.Type y.Type)
            | Some(meth) ->
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)    
    | rexp.NameCall(longName, genericArgs, args, pos) -> //todo: need more position info for different tokens
        let namePrefix, methodName =
            let split = longName.Split('.')
            String.Join(".",split.[..split.Length-2]), split.[split.Length-1]
        let args = args |> List.map (tycheck)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray

        match Map.tryFind namePrefix env.Variables with //N.B. vars always supercede open names
        | Some(ty:Type) -> //instance method call on variable
            match tryResolveMethodWithGenericArgs ty methodName instanceFlags (genericArgs |> List.toArray) argTys pos with
            | None -> semError pos (sprintf "not a valid instance method: %s, for the given instance type: %s, and arg types: %A" methodName ty.Name argTys)
            | Some(meth) -> texp.InstanceCall(Var(namePrefix,ty), meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | None ->
            match tryResolveType (TySig(namePrefix,[])) with
            | Some(ty) -> //static method call (possibly generic) on non-generic type (need to handle generic type in another parse case, i think)
                match  tryResolveMethodWithGenericArgs ty methodName staticFlags (genericArgs |> List.toArray) argTys pos with
                | None -> semError pos (sprintf "not a valid static method: %s, for the given class type: %s, and arg types: %A" methodName ty.Name argTys)
                | Some(meth) -> texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
            | None -> //constructors
                match tryResolveType (TySig(longName,genericArgs)) with
                | None -> semError pos (sprintf "could not resolve method call type: %s or constructor type: %s" namePrefix longName)
                | Some(ty) ->
                    if ty.IsValueType && args.Length = 0 then
                        if ty = typeof<System.Void> then
                            semError pos (sprintf "System.Void cannot be instantiated")
                        else
                            texp.Default(ty)
                    else
                        match ty.GetConstructor(argTys) with
                        | null -> semError pos (sprintf "could not resolve constructor for type: %s with arg types: %A" ty.Name (args |> List.map(fun arg -> arg.Type)))
                        | ctor -> texp.Ctor(ctor, castArgsIfNeeded (ctor.GetParameters()) args, ty)
    | rexp.GenericTypeStaticCall(tyName, tyGenericArgs, methodName, methodGenericArgs, args, pos) -> //todo: need more position info for different tokens
        match tryResolveType (TySig(tyName, tyGenericArgs)) with
        | None -> semError pos (sprintf "could not resolve type: %s with generic arg types: %A" tyName tyGenericArgs)
        | Some(ty) ->
            let args = args |> List.map (tycheck)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            match tryResolveMethodWithGenericArgs ty methodName staticFlags (methodGenericArgs |> List.toArray) argTys pos with
            | None -> semError pos (sprintf "not a valid static method: %s, for the given class type: %s, and arg types: %A" methodName ty.Name argTys)
            | Some(meth) -> 
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
    | rexp.ExpCall(instance, methodName, methodGenericArgs, args, pos) ->
        let instance = tycheck instance
        let args = args |> List.map (tycheck)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        match tryResolveMethodWithGenericArgs instance.Type methodName instanceFlags (methodGenericArgs |> List.toArray) argTys pos with
        | None -> semError pos (sprintf "not a valid instace method: %s, for the given expression type: %s, and arg types: %A" methodName instance.Type.Name argTys)
        | Some(meth) ->
            texp.InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
    | rexp.Let(name, assign, body, pos) ->
        let assign = tycheck assign
        if assign.Type = typeof<Void> then
            semError pos (sprintf "System.Void is not a valid value in a let binding")
        let body = tycheckWith {env with Variables=env.Variables |> Map.add name assign.Type} body
        texp.Let(name,assign, body, body.Type)
    | rexp.Var(name, pos) ->
        match Map.tryFind name env.Variables with
        | Some(ty) -> texp.Var(name,ty)
        | None -> semError pos (sprintf "Var not found in environment: %s" name)
    | rexp.Sequential((rexp.Break(_)|rexp.Continue(_)),_, pos) ->
        semError pos (sprintf "unreachable code detected")
    | rexp.Sequential(x,y, pos) ->
        let x, y = tycheck x, tycheck y
        texp.Sequential(x,y,y.Type)
    | rexp.Open(name, x, pos) ->
        let exists =
            env.Assemblies
            |> Seq.collect (fun asm -> asm.GetTypes() |> Seq.map (fun ty -> let ns = ty.Namespace in if ns = null then "" else ns.ToLower()))
            |> Seq.exists ((=)name)

        if not exists then
            semError pos (sprintf "namespace %s does not exist in any currently open assemblies: %A" name env.Assemblies)

        tycheckWith {env with Namespaces=name::env.Namespaces} x
    | rexp.Ref(name, x, pos) ->
        let asm =
            try
                Assembly.Load(name)
            with _ ->
                try 
                    //are we sure we don't want to use LoadFile so that we can use reference in different scopes?
                    Assembly.LoadFrom(name)
                with _ ->
                    semError pos (sprintf "Unable to resolve assembly reference: %s" name)
        tycheckWith {env with Assemblies=asm::env.Assemblies} x
    | rexp.Not(x,pos) ->
        let x = tycheck x
        if x.Type <> typeof<bool> then
            semError pos "Not expression must be bool"
        else
            texp.Not(x, x.Type)
    | rexp.Cast(x,ty,pos) ->
        let x = tycheck x
        match tryResolveType ty with
        | None -> semError pos (sprintf "could not resolve cast type: %A" ty)
        | Some(ty) ->
            if ty = typeof<System.Void> then
                semError pos (sprintf "casting to Sytem.Void will always fail")
            elif x.Type = ty then
                semError pos (sprintf "casting a value to itself own type (%s) is a no-op" x.Type.Name)
            elif ty.IsAssignableFrom(x.Type) || x.Type.IsAssignableFrom(ty) then
                texp.Cast(x,ty)
            elif (x.Type = typeof<int> && ty = typeof<float>) || (x.Type = typeof<float> && ty = typeof<int>) then
                texp.Coerce(x,ty) 
            else
                let meth = seq {
                    //giver implicit conversion op from either type over explicit ops; next prefer conversion op defined on lhs type over rhs type
                    yield tryResolveOpImplicit ty x.Type ty
                    yield tryResolveOpImplicit x.Type x.Type ty
                    yield tryResolveOpExplicit ty x.Type ty
                    yield tryResolveOpExplicit x.Type x.Type ty } |> Seq.tryPick id

                match meth with
                | Some(meth) -> texp.StaticCall(meth, [x], meth.ReturnType)    
                | None -> semError pos (sprintf "a cast from type %s to the type %s will always fail" x.Type.Name ty.Name)
    | rexp.IfThenElse(x,y,z,pos) ->
        let x = tycheck x
        if x.Type <> typeof<bool> then
            semError pos (sprintf "test expresion must be boolean not %s" x.Type.Name)
        
        let y = tycheck y
        match z with
        | Some(z) ->
            let z = tycheck z
            if y.Type <> z.Type then
                semError pos (sprintf "then and else branches must be of same type but instead are %s and %s" y.Type.Name z.Type.Name)
            texp.IfThenElse(x,y,z,y.Type)
        | None ->
            if y.Type = typeof<Void> then
                texp.IfThen(x,y)
            else
                texp.IfThenElse(x, y, texp.Default(y.Type), y.Type)
    | rexp.ComparisonBinop(op, x, y, pos) ->
        let x, y = tycheck x, tycheck y

        if x.Type = typeof<int> && y.Type = typeof<float> then
            texp.ComparisonBinop(op, texp.Coerce(x,typeof<float>), y)
        elif x.Type = typeof<float> && y.Type = typeof<int> then
            texp.ComparisonBinop(op, x, texp.Coerce(y,typeof<float>))
        else
            let opName =
                match op with
                | Eq -> "op_Equality"
                | Lt -> "op_LessThan"
                | Gt -> "op_GreaterThan"
            
            let meth = seq {
                yield tryResolveMethod x.Type opName staticFlags [||] [|x.Type; y.Type|]
                yield tryResolveMethod y.Type opName staticFlags [||] [|x.Type; y.Type|] } |> Seq.tryPick id

            match meth with
            | Some(meth) ->
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)    
            | None when op = Eq && x.Type.IsAssignableFrom(y.Type) || y.Type.IsAssignableFrom(x.Type) -> //we know value types are not involved
                texp.ComparisonBinop(op, x, y)    
            | None ->
                semError pos (sprintf "No overloads found for binary operator %A with left-hand-side type %A and right-hand-side type %A" op x.Type y.Type)
    | rexp.Nop _ ->
        texp.Nop
    | rexp.VarSet(name, x, pos) ->
        let x = tycheck x
        match Map.tryFind name env.Variables with
        | Some(ty) -> 
            if x.Type <> ty then
                semError pos (sprintf "Var %s of type '%s' is not the same type as the expression being assigned, '%s'" name ty.Name x.Type.Name)
            else
                texp.VarSet(name, x)
        | None -> semError pos (sprintf "Var not found in environment: %s" name)
    | rexp.WhileLoop(condition, body, pos) ->
        let condition = tycheck condition
        if condition.Type <> typeof<bool> then
            semError pos (sprintf "while loop condition must be of type 'bool' but instead is of type '%s'" condition.Type.Name)
        else
            let body = tycheckWith {env with IsLoopBody=true} body
            texp.WhileLoop(condition, body)
    | rexp.Break(pos) ->
        if not env.IsLoopBody then
            semError pos (sprintf "'break()' is only valid inside a loop body")
        else
            texp.Break
    | rexp.Continue(pos) ->
        if not env.IsLoopBody then
            semError pos (sprintf "'continue()' is only valid inside a loop body")
        else
            texp.Continue
//
//        