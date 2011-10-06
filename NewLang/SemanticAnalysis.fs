module Swensen.NewLang.SemanticAnalysis

open System
open System.Reflection

module EM = ErrorMessage
let abort() = raise CompilerInterruptException

let sprintSeqForDisplay xs f =
    if xs = Seq.empty then "()" 
    else (xs |> Seq.map (fun x -> sprintf "'%s'" (f x)) |> String.concat ", ")

let sprintTypes (tarr:Type seq) =
    sprintSeqForDisplay tarr (fun ty -> ty.Name)

let sprintTySigs (tarr:tySig seq) =
    sprintSeqForDisplay tarr (fun ty -> ty.DisplayValue)

let sprintAssemblies (tarr:Assembly seq) =
    sprintSeqForDisplay tarr (fun asm -> asm.FullName)

///Binding flags for our language
let instanceFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.IgnoreCase
let staticFlags = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.IgnoreCase

let coerceIfNeeded expectedTy (targetExp:texp) =
    if targetExp.Type <> expectedTy then Coerce(targetExp,expectedTy) else targetExp

let coerceEachIfNeeded expectedTys targetExps =
    List.zip expectedTys targetExps
    |> List.map (fun (targetTy, targetExp) -> coerceIfNeeded targetTy targetExp)

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
            | None -> 
                EM.Could_not_resolve_types pos (sprintTySigs genericArgs)
                abort()
            | Some(genericArgTys) -> tryResolveMethod ty methodName bindingFlags genericArgTys argTys

    match rawExpression with
    | rexp.Double x -> texp.Double x
    | rexp.Int32 x  -> texp.Int32 x
    | rexp.String x -> texp.String x
    | rexp.Char x   -> texp.Char x
    | rexp.Bool x   -> texp.Bool x
    | rexp.Null(name, pos)   -> 
        match tryResolveType name with
        | None -> 
            EM.Could_not_resolve_type pos name.DisplayValue //todo: specific pos for ty name
            abort()
        | Some(ty) ->
            if ty.IsValueType then
                EM.Null_is_invalid_for_value_types pos ty.Name
                texp.Null(ty) //error recovery: use wants to use a ValueType, but incorrectly wanted to use null for it
            else
                texp.Null(ty)
    | rexp.Typeof(name, pos)   -> 
        match tryResolveType name with
        | None -> 
            EM.Could_not_resolve_type pos name.DisplayValue
            texp.Typeof(typeof<obj>) //error recovery: this is a runtime value that won't hurt us error 
        | Some(ty) -> 
            texp.Typeof(ty)
    | rexp.UMinus(x,pos) ->
        let x = tycheck x
        texp.UMinus(x,x.Type)
    | rexp.Fact(x,pos) ->
        let x = tycheck x
        if x.Type <> typeof<int> then
            EM.Expected_type_but_got_type pos "System.Int32" x.Type.Name
            texp.Int32(0) //error recovery: return type of Fact is always Int32
        else
            let meth = typeof<CoreOps>.GetMethod("Factorial",[|typeof<int>|])
            texp.StaticCall(meth, [x], meth.ReturnType)
    | rexp.Pow(x,y,pos) ->
        let x, y = tycheck x, tycheck y
        //TODO: hmm, revisit this, i'm not so sure we want to pass in static types instead of true types of x and y, we know this should resolve
        let meth = typeof<System.Math>.GetMethod("Pow",[|typeof<float>;typeof<float>|])
//        if meth = null then
//            semError pos (EM.Invalid_static_method "Pow" "System.Math" (sprintTypes [x.Type;y.Type]))
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
            if meth = null then //TODO: TRY RESOLVE AND PRODUCE ERROR MESSAGE IN ONE STROKE
                EM.Invalid_static_method pos "Concat" "System.String" (sprintTypes [|x.Type; y.Type|])
                texp.String("") //error recovery
            else
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)
        | None -> //static "op_*" overloads
            let meth = seq {
                yield tryResolveMethod x.Type op.Name staticFlags [||] [|x.Type; y.Type|]
                yield tryResolveMethod y.Type op.Name staticFlags [||] [|x.Type; y.Type|] } |> Seq.tryPick id

            match meth with
            | None ->
                EM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                texp.Error(x.Type) //error recovery: best guess of intended return type
            | Some(meth) ->
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType) 
    | rexp.ComparisonBinop(op, x, y, pos) ->
        let x, y = tycheck x, tycheck y
        
        //todo: refactor this tower stuff!
        let tower = [(typeof<int>, 1); (typeof<float>, 2)]
        let heightInTower inputTy = tower |> List.tryPick (fun (towerTy, height) -> if inputTy = towerTy then Some(height) else None)
        let towerTy = 
            match heightInTower x.Type, heightInTower y.Type with
            | Some(xheight), Some(yheight) ->
                Some(if xheight > yheight then x.Type else y.Type)
            | _ -> None
                
        //first numeric tower value type cases
        match towerTy with
        | Some(towerTy) ->
            texp.ComparisonBinop(op, coerceIfNeeded towerTy x, coerceIfNeeded towerTy y)
        | None ->
            //next operator overloads
            let meth = seq {
                yield tryResolveMethod x.Type op.Name staticFlags [||] [|x.Type; y.Type|]
                yield tryResolveMethod y.Type op.Name staticFlags [||] [|x.Type; y.Type|] } |> Seq.tryPick id

            match meth, op with
            | Some(meth), _ ->
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)    
            | None, (Eq | Neq) when (x.Type.IsAssignableFrom(y.Type) || y.Type.IsAssignableFrom(x.Type)) && (not (x.Type.IsValueType <> y.Type.IsValueType)) -> //reference equals
                texp.ComparisonBinop(op, x, y)    
            | None, _ ->
                EM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                texp.Error(typeof<bool>)
    | rexp.NameCall(longName, genericArgs, args, pos) -> //todo: need more position info for different tokens
        let namePrefix, methodName =
            let split = longName.Split('.')
            String.Join(".",split.[..split.Length-2]), split.[split.Length-1]
        let args = args |> List.map (tycheck)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray

        match Map.tryFind namePrefix env.Variables with //N.B. vars always supercede open names
        | Some(ty:Type) -> //instance method call on variable
            match tryResolveMethodWithGenericArgs ty methodName instanceFlags (genericArgs |> List.toArray) argTys pos with
            | None -> 
                EM.Invalid_instance_method pos methodName ty.Name (sprintTypes argTys)
                abort()
            | Some(meth) -> 
                texp.InstanceCall(Var(namePrefix,ty), meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | None ->
            match tryResolveType (TySig(namePrefix,[])) with
            | Some(ty) -> //static method call (possibly generic) on non-generic type (need to handle generic type in another parse case, i think)
                match  tryResolveMethodWithGenericArgs ty methodName staticFlags (genericArgs |> List.toArray) argTys pos with
                | None -> 
                    EM.Invalid_static_method pos methodName ty.Name (sprintTypes argTys)
                    abort()
                | Some(meth) -> 
                    texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
            | None -> //constructors
                match tryResolveType (TySig(longName,genericArgs)) with
                | None -> 
                    EM.Could_not_resolve_possible_method_call_or_contructor_type pos namePrefix longName
                    abort()
                | Some(ty) ->
                    if ty.IsValueType && args.Length = 0 then
                        if ty = typeof<System.Void> then
                            EM.Void_cannot_be_instantiated pos
                            texp.Error(ty)
                        else
                            texp.Default(ty)
                    else
                        match ty.GetConstructor(argTys) with
                        | null -> 
                            EM.Could_not_resolve_constructor pos ty.Name (args |> List.map(fun arg -> arg.Type) |> sprintTypes)
                            texp.Error(ty)
                        | ctor -> 
                            texp.Ctor(ctor, castArgsIfNeeded (ctor.GetParameters()) args, ty)
    | rexp.GenericTypeStaticCall(tyName, tyGenericArgs, methodName, methodGenericArgs, args, pos) -> //todo: need more position info for different tokens
        match tryResolveType (TySig(tyName, tyGenericArgs)) with
        | None -> 
            EM.Could_not_resolve_type pos (TySig(tyName,tyGenericArgs).DisplayValue)
            abort()
        | Some(ty) ->
            let args = args |> List.map (tycheck)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            match tryResolveMethodWithGenericArgs ty methodName staticFlags (methodGenericArgs |> List.toArray) argTys pos with
            | None -> 
                EM.Invalid_static_method pos methodName ty.Name (sprintTypes argTys)
                abort()
            | Some(meth) -> 
                texp.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
    | rexp.ExpCall(instance, methodName, methodGenericArgs, args, pos) ->
        let instance = tycheck instance
        let args = args |> List.map (tycheck)
        let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
        match tryResolveMethodWithGenericArgs instance.Type methodName instanceFlags (methodGenericArgs |> List.toArray) argTys pos with
        | None -> 
            EM.Invalid_instance_method pos methodName instance.Type.Name (sprintTypes argTys)
            abort()
        | Some(meth) ->
            texp.InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
    | rexp.Let(name, assign, body, pos) ->
        let assign = tycheck assign
        if assign.Type = typeof<Void> then
            EM.Void_invalid_in_let_binding pos
        
        let body = tycheckWith {env with Variables=env.Variables |> Map.add name assign.Type} body
        texp.Let(name,assign, body, body.Type)
    | rexp.Var(name, pos) ->
        match Map.tryFind name env.Variables with
        | Some(ty) -> 
            texp.Var(name,ty)
        | None -> 
            EM.Variable_not_found pos name
            abort()
    | rexp.Sequential((rexp.Break(_)|rexp.Continue(_)), _, pos) ->
        EM.Unreachable_code_detected pos
        abort() //not sure what the best error recovery might be
    | rexp.Sequential(x,y, pos) ->
        let x, y = tycheck x, tycheck y
        texp.Sequential(x,y,y.Type)
    | rexp.OpenNamespace(name, x, pos) ->
        let exists =
            env.Assemblies
            |> Seq.collect (fun asm -> asm.GetTypes() |> Seq.map (fun ty -> let ns = ty.Namespace in if ns = null then "" else ns.ToLower()))
            |> Seq.exists ((=)name)

        if not exists then
            EM.Namespace_not_found pos name (sprintAssemblies env.Assemblies)
            tycheck x
        else
            tycheckWith {env with Namespaces=name::env.Namespaces} x
    | rexp.OpenAssembly(name, x, pos) ->
        let asm =
            try
                Some(Assembly.Load(name))
            with _ ->
                try 
                    //are we sure we don't want to use LoadFile so that we can use reference in different scopes?
                    Some(Assembly.LoadFrom(name))
                with _ ->
                    None
        match asm with
        | None -> 
            EM.Could_not_resolve_assembly pos name
            tycheck x
        | Some(asm) -> tycheckWith {env with Assemblies=asm::env.Assemblies} x
    | rexp.Not(x,pos) ->
        let x = tycheck x
        if x.Type <> typeof<bool> then
            EM.Expected_type_but_got_type pos "System.Boolean" x.Type.Name
            texp.Error(typeof<bool>)
        else
            texp.Not(x, x.Type)
    | rexp.Cast(x,ty,pos) ->
        let x = tycheck x
        match tryResolveType ty with
        | None -> 
            EM.Could_not_resolve_type pos ty.DisplayValue
            abort()
        | Some(ty) ->
            if ty = typeof<System.Void> then
                EM.Casting_to_void_invalid pos
                texp.Error(ty)
            elif x.Type = ty then
                EM.Casting_noop pos x.Type.Name
                x
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
                | Some(meth) -> 
                    texp.StaticCall(meth, [x], meth.ReturnType)    
                | None -> 
                    EM.Casting_from_type_to_type_always_invalid pos x.Type.Name ty.Name
                    texp.Error(ty)
    | rexp.Xor((x,xpos),(y,ypos)) ->
        let x =
            match tycheck x with
            | x when x.Type <> typeof<bool> -> 
                EM.Expected_type_but_got_type xpos "System.Bool" x.Type.Name
                texp.Error(typeof<bool>)
            | x -> x

        let y = 
            match tycheck y with
            | y when y.Type <> typeof<bool> ->
                EM.Expected_type_but_got_type ypos "System.Bool" y.Type.Name
                texp.Error(typeof<bool>)
            | y -> y
        texp.Xor(x,y)
    | rexp.IfThenElse(condition,thenBranch,elseBranch,pos) ->
        let condition = 
            let condition = tycheck condition
            if condition.Type <> typeof<bool> then
                EM.Expected_type_but_got_type pos "System.Boolean" condition.Type.Name
                texp.Error(typeof<bool>)
            else
                condition
        let thenBranch = tycheck thenBranch
        match elseBranch with
        | Some(elseBranch) ->
            let elseBranch = tycheck elseBranch
            if thenBranch.Type <> elseBranch.Type then
                //maybe could use 
                EM.IfThenElse_branch_type_mismatch pos thenBranch.Type.Name elseBranch.Type.Name
                texp.IfThenElse(condition,thenBranch, texp.Error(thenBranch.Type), thenBranch.Type)
            else
                texp.IfThenElse(condition,thenBranch,elseBranch,thenBranch.Type)
        | None ->
            if thenBranch.Type = typeof<Void> then
                texp.IfThen(condition,thenBranch)
            else
                texp.IfThenElse(condition, thenBranch, texp.Default(thenBranch.Type), thenBranch.Type)
    | rexp.Nop _ ->
        texp.Nop
    | rexp.VarSet(name, x, pos) ->
        let x = tycheck x
        match Map.tryFind name env.Variables with
        | Some(ty) -> 
            if x.Type <> ty then
                EM.Variable_set_type_mismatch pos name ty.Name x.Type.Name
                texp.Error(typeof<Void>)
            else
                texp.VarSet(name, x)
        | None -> 
            EM.Variable_not_found pos name
            texp.Error(typeof<Void>)
    | rexp.WhileLoop(condition, body, pos) ->
        let condition = 
            let condition = tycheck condition
            if condition.Type <> typeof<bool> then
                EM.Expected_type_but_got_type pos "System.Boolean" condition.Type.Name //todo: NEED PRECISE POSITION INFO HERE!
                texp.Error(typeof<bool>)
            else
                condition
        let body = tycheckWith {env with IsLoopBody=true} body
        texp.WhileLoop(condition, body)
    | rexp.Break(pos) ->
        if not env.IsLoopBody then
            EM.Break_outside_of_loop pos
            texp.Error(typeof<Void>)
        else
            texp.Break
    | rexp.Continue(pos) ->
        if not env.IsLoopBody then
            EM.Continue_outside_of_loop pos
            texp.Error(typeof<Void>)
        else
            texp.Continue
//
//        