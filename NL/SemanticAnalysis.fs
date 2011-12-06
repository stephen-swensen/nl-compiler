module Swensen.NL.SemanticAnalysis

open System
open System.Reflection
open Swensen.NL.Ast
open Swensen.NL.Ail

///Functions for calculating primitive type widening
module NumericTower =
        //todo: refactor this tower stuff!
    let private tower = [(typeof<int>, 1); (typeof<float>, 2)]
    let heightInTower inputTy = tower |> List.tryPick (fun (towerTy, height) -> if inputTy = towerTy then Some(height) else None)
    let tallestTy xTy yTy = 
        match heightInTower xTy, heightInTower yTy with
        | Some(xheight), Some(yheight) ->
            Some(if xheight > yheight then xTy else yTy)
        | _ -> None

module EM = ErrorMessage
let abort() = raise CompilerInterruptException

let sprintSeqForDisplay xs f =
    if xs = Seq.empty then "()" 
    else (xs |> Seq.map (fun x -> sprintf "'%s'" (f x)) |> String.concat ", ")

let sprintTypes (tarr:Type seq) =
    sprintSeqForDisplay tarr (fun ty -> ty.Name)

let sprintTySigs (tarr:Ast.TySig seq) =
    sprintSeqForDisplay tarr (fun ty -> ty.Name)

let sprintAssemblies (tarr:Assembly seq) =
    sprintSeqForDisplay tarr (fun asm -> asm.FullName)

///Binding flags for our language
let instanceFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.IgnoreCase
let staticFlags = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.IgnoreCase

let coerceIfNeeded expectedTy (targetExp:ILExpr) =
    if targetExp.Type <> expectedTy then Coerce(targetExp,expectedTy) else targetExp

let castIfNeeded expectedTy (targetExp:ILExpr) =
    if targetExp.Type <> expectedTy then Cast(targetExp,expectedTy) else targetExp

///SHOULD MAKE A "CAST OR COERCE IF NEEDED" METHOD!! BUG 31 
let castArgsIfNeeded (expectedParameters:ParameterInfo[]) targetExps =
    List.zip (expectedParameters |> Seq.map (fun p -> p.ParameterType) |> Seq.toList)  targetExps
    |> List.map (fun (targetTy, targetExp) -> castIfNeeded targetTy targetExp)

//todo: infer generic type arguments from type parameters (reflection not friendly for this)
//todo: file bug: name should not need a type constraint
///Try to resolve to resolve a method with the given parameters
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

///first param is the type name, second parameter is the namesapce to prefix to the type name
let withNamespace tyName ns =
    if ns = "" then tyName
    else ns + "." + tyName

///Try to resolve the given type in the refAsms and openNames context; return null if fail to resolve.
///If exact is true, does not search for the generic type name among namespaces (though generic type parameters will still be resolved among namespaces)
let rec tryResolveTypeExact (env:SemanticEnvironment) (gsig:TySig) exact =
    match gsig with
    | _ when gsig.Cached <> null -> Some(gsig.Cached)
    | Ast.TySig(name,args) ->
        seq {
            let nsl = if exact then Seq.singleton name else env.Namespaces |> Seq.map (withNamespace name)
            for possibleName in nsl do
                for possibleAsm in env.Assemblies do
                    if args = [] then
                        let possibleFullName = possibleName + ", " + possibleAsm.FullName
                        let ty = Type.GetType(possibleFullName,false, true)
                        gsig.Cached <- ty
                        yield Option.ofAllowsNull ty
                    else
                        match tryResolveGenericArgTys env args with
                        | None -> yield None
                        | Some(argTys) ->
                            let possibleFullName = 
                                sprintf "%s`%i[%s], %s" 
                                    possibleName 
                                    args.Length 
                                    (String.concat "," (argTys |> Seq.map (fun (argTy:Type) -> sprintf "[%s]" argTy.AssemblyQualifiedName)))
                                    possibleAsm.FullName
                            let ty = Type.GetType(possibleFullName,false, true)
                            gsig.Cached <- ty
                            yield Option.ofAllowsNull ty
        } |> Seq.tryPick id
///Try to resolve the given type in the refAsms and openNames context; return null if fail to resolve.
and tryResolveType env gsig = tryResolveTypeExact env gsig false
//todo: make cleaner, either wait to convert to reflection friendly arrays till last moment, or convert to arrays up front
//////Try to resolve the given sequence of generic argument types in the refAsms and openNames context; return null if fail to resolve.
and tryResolveGenericArgTys env (genericArgs:seq<_>) =        
    let genericArgTys = genericArgs |> Seq.map (tryResolveType env)
    if Seq.exists ((=)None) genericArgTys then None
    else Some(genericArgTys |> Seq.choose id |> Seq.toArray)

//todo: don't like passing in pos here too much
let tryResolveMethodWithGenericArgs env ty methodName bindingFlags genericArgs argTys pos =
    match genericArgs with
    | [||] ->
        tryResolveMethod ty methodName bindingFlags [||] argTys
    | genericArgs -> 
        match tryResolveGenericArgTys env genericArgs with
        | None -> 
            EM.Could_not_resolve_types pos (sprintTySigs genericArgs)
            abort()
        | Some(genericArgTys) -> tryResolveMethod ty methodName bindingFlags genericArgTys argTys

let tryResolveField (ty:Type) fieldName bindingFlags =
    Option.ofAllowsNull <| ty.GetField(fieldName, bindingFlags)

//only resolves simple non-parameterized properties
let tryResolveProperty (ty:Type) (propertyName:string) (bindingFlags:BindingFlags) =
    Option.ofAllowsNull <| ty.GetProperty(propertyName, bindingFlags)

///Tests whethere the given namespace exists in  the list of assemblies
let namespaceExists (assemblies: Assembly list) name =
    assemblies
    |> Seq.collect (fun asm -> asm.GetTypes() |> Seq.map (fun ty -> let ns = ty.Namespace in if ns = null then "" else ns.ToLower()))
    |> Seq.exists ((=)name)

///try to load the given assembly either by file path or meta name
let tryLoadAssembly (name:string) =
    try
        Some(Assembly.Load(name))
    with _ ->
        try 
            //are we sure we don't want to use LoadFile so that we can use reference in different scopes?
            Some(Assembly.LoadFrom(name))
        with _ ->
            None

type FP =
    | Field of FieldInfo //N.B. CONSTANT FIELDS LIKE INT32.MAXVALUE CANNOT BE ACCESSED NORMALLY: CHECK FOR ISLITERAL FIELD ATTRIBUTE AND EMIT LITERAL VALUE (CAN USE GETVALUE REFLECTION) -- IF CAN'T DO THAT, CHECK FIELDINFO HANDLE AND IF THROWS WE KNOW WE NEED TO STOP
    | Property of PropertyInfo

let tryResolveFieldOrProperty ty name bindingFlags =
    match tryResolveField ty name staticFlags with
    | Some(fi) -> Some(FP.Field(fi))
    | None ->
        match tryResolveProperty ty name staticFlags with
        | Some(pi) -> Some(FP.Property(pi))
        | None -> None

type VFP =
    | Var of string * Type
    | FieldOrProperty of FP

let tryResolveVarFieldOrProperty env (ident:Identifier) =
    let rec loop = function
        | [] -> None
        | hd::tl ->
            match hd with
            | NVT.Variable(name,ty) when name = ident.Full -> //local var
                Some(VFP.Var(name,ty))
            | NVT.Namespace(ns) when ident.IsLong ->
                match tryResolveTypeExact env (Ast.TySig(withNamespace ident.LongPrefix ns, [])) true with
                | Some(ty) ->
                    match tryResolveFieldOrProperty ty ident.ShortSuffix staticFlags with
                    | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                    | None -> loop tl
                | None -> loop tl
            | NVT.Type(ty) when ident.IsShort -> //field or property of an open type
                match tryResolveFieldOrProperty ty ident.ShortSuffix staticFlags with
                | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                | None -> loop tl
            | _ -> loop tl
    loop env.NVTs

///Symantic analysis (type checking)
let rec tycheckWith env synTopLevel =
    let rec tycheckExpWith env synExpr=
        let tycheckExp = tycheckExpWith env

        match synExpr with
        | Ast.SynExpr.Double x -> ILExpr.Double x
        | Ast.SynExpr.Int32 x  -> ILExpr.Int32 x
        | Ast.SynExpr.String x -> ILExpr.String x
        | Ast.SynExpr.Char x   -> ILExpr.Char x
        | Ast.SynExpr.Bool x   -> ILExpr.Bool x
        | Ast.SynExpr.Null(name, pos)   -> 
            match tryResolveType env name with
            | None -> 
                EM.Could_not_resolve_type pos name.Name //todo: specific pos for ty name
                abort()
            | Some(ty) ->
                if ty.IsValueType then
                    EM.Null_is_invalid_for_value_types pos ty.Name
                    ILExpr.Null(ty) //error recovery: use wants to use a ValueType, but incorrectly wanted to use null for it
                else
                    ILExpr.Null(ty)
        | Ast.SynExpr.Typeof(name, pos)   -> 
            match tryResolveType env name with
            | None -> 
                EM.Could_not_resolve_type pos name.Name
                ILExpr.Typeof(typeof<obj>) //error recovery: this is a runtime value that won't hurt us error 
            | Some(ty) -> 
                ILExpr.Typeof(ty)
        | Ast.SynExpr.Default(name, pos)   -> 
            match tryResolveType env name with
            | None -> 
                EM.Could_not_resolve_type pos name.Name
                abort()
            | Some(ty) -> 
                if ty = typeof<System.Void> then
                    EM.Void_cannot_be_instantiated pos
                    ILExpr.Default(ty) //error recovery
                else
                    ILExpr.Default(ty)
        | Ast.SynExpr.UMinus(x,pos) ->
            let x = tycheckExp x
            if x.Type = typeof<Int64> ||
               x.Type = typeof<Int32> ||
               x.Type = typeof<Int16> ||
               x.Type = typeof<Double> ||
               x.Type = typeof<Single> 
            then
               ILExpr.UMinus(x, x.Type)
            else           
                match x.Type.GetMethod("op_UnaryNegation") with
                | null ->
                    EM.No_overload_found_for_unary_operator pos "-" x.Type.Name
                    ILExpr.Error(x.Type)
                | meth ->
                    ILExpr.StaticCall(meth, [x], meth.ReturnType)
        | Ast.SynExpr.Pow(x, y, pos) ->        
            let x,y = tycheckExp x, tycheckExp y
            //TODO: hmm, revisit this, i'm not so sure we want to pass in static types instead of true types of x and y, we know this should resolve
            match tryResolveMethod typeof<System.Math> "Pow" staticFlags [||] [|typeof<float>;typeof<float>|] with
            | None -> 
                EM.Internal_error pos "Failed to resolve 'System.Math.Pow(float,float)' for synthetic operator '**'"
                ILExpr.Error(typeof<float>)
            | Some(meth) ->
                let canCoerceToFloat (arg:ILExpr) = //TODO: UNIT TEST THESE CASES
                    let floatHeight = (NumericTower.heightInTower typeof<float>).Value //assert?
                    match NumericTower.heightInTower arg.Type with
                    | Some(argheight) when argheight <= floatHeight -> true
                    | _ -> false

                if canCoerceToFloat x && canCoerceToFloat y then
                    ILExpr.StaticCall(meth, [x;y] |> List.map (coerceIfNeeded typeof<float>) , meth.ReturnType)
                else
                    EM.No_overload_found_for_binary_operator pos "**" x.Type.Name y.Type.Name
                    ILExpr.Error(typeof<float>)
        | Ast.SynExpr.NumericBinop(op,x,y,pos) ->
            let x, y = tycheckExp x, tycheckExp y
            match NumericTower.tallestTy x.Type y.Type with
            | Some(tallestTy) -> //primitive
                ILExpr.mkNumericBinop(op, coerceIfNeeded tallestTy x, coerceIfNeeded tallestTy y, tallestTy)
            | None when op = Ast.SynNumericBinop.Plus && (x.Type = typeof<string> || y.Type = typeof<string>) -> //string
                let meth = tryResolveMethod typeof<System.String> "Concat" staticFlags [||] [|x.Type; y.Type|]
                match meth with
                | None ->
                    //there should always be a String.Concat(obj,obj) overload
                    EM.Internal_error pos (sprintf "Could not resolve 'String.Concat' synthetic '+' overload for argument types %s" (sprintTypes [x.Type; y.Type]))
                    ILExpr.Error(typeof<string>) //error recovery
                | Some(meth) ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)
            | None -> //static "op_*" overloads
                let meth = seq {
                    yield tryResolveMethod x.Type op.Name staticFlags [||] [|x.Type; y.Type|]
                    yield tryResolveMethod y.Type op.Name staticFlags [||] [|x.Type; y.Type|] } |> Seq.tryPick id

                match meth with
                | None ->
                    EM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                    ILExpr.Error(x.Type) //error recovery: best guess of intended return type
                | Some(meth) ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType) 
        | Ast.SynExpr.ComparisonBinop(op, x, y, pos) ->
            let x, y = tycheckExp x, tycheckExp y
                        
            //first numeric tower value type cases
            match NumericTower.tallestTy x.Type y.Type with
            | Some(tallestTy) ->
                ILExpr.mkComparisonBinop(op, coerceIfNeeded tallestTy x, coerceIfNeeded tallestTy y)
            | None ->
                //next operator overloads
                let meth = seq {
                    yield tryResolveMethod x.Type op.Name staticFlags [||] [|x.Type; y.Type|]
                    yield tryResolveMethod y.Type op.Name staticFlags [||] [|x.Type; y.Type|] } |> Seq.tryPick id

                match meth, op with
                | Some(meth), _ ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y], meth.ReturnType)    
                //reference equals
                | None, (Ast.SynComparisonBinop.Eq | Ast.SynComparisonBinop.Neq) when (x.Type.IsAssignableFrom(y.Type) || y.Type.IsAssignableFrom(x.Type)) && (not (x.Type.IsValueType <> y.Type.IsValueType)) -> 
                    ILExpr.mkComparisonBinop(op, x, y)    
                | None, _ ->
                    EM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                    ILExpr.Error(typeof<bool>)
        //this is our most complex part of the grammer...
        | Ast.SynExpr.NameCall(ident, (genericArgs, genericArgsPos), args, pos) ->
            let args = args |> List.map (tycheckExp)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray

            let rec loop = function
                | [] -> 
                    EM.Could_not_resolve_possible_method_call_or_contructor_type pos ident.LongPrefix ident.Full //TODO: may not be accurate any more!
                    abort() //need error message too!
                | hd::tl ->
                    match hd with
                    | NVT.Variable(name,ty) -> //instance method call on variable
                        if name = ident.LongPrefix then
                            match tryResolveMethodWithGenericArgs env ty ident.ShortSuffix instanceFlags (genericArgs |> List.toArray) argTys genericArgsPos with
                            | None -> 
                                EM.Invalid_instance_method pos ident.ShortSuffix ty.Name (sprintTypes argTys)
                                abort()
                            | Some(meth) -> 
                                ILExpr.InstanceCall(ILExpr.Var(ident.LongPrefix,ty), meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
                        else 
                            loop tl
                    | NVT.Namespace(ns) ->
                        match tryResolveTypeExact env (Ast.TySig(withNamespace ident.LongPrefix ns, [])) true with
                        | Some(ty) -> //static method call (possibly generic) on non-generic type (need to handle generic type in another parse case, i think)
                            match tryResolveMethodWithGenericArgs env ty ident.ShortSuffix staticFlags (genericArgs |> List.toArray) argTys genericArgsPos with
                            | None -> 
                                EM.Invalid_static_method pos ident.ShortSuffix ty.Name (sprintTypes argTys)
                                abort()
                            | Some(meth) -> 
                                ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
                        | None -> //constructors (generic or non-generic)
                            match tryResolveTypeExact env (Ast.TySig(withNamespace ident.Full ns,genericArgs)) true with
                            | None -> 
                                loop tl
                            | Some(ty) ->
                                if ty.IsValueType && args.Length = 0 then
                                    if ty = typeof<System.Void> then
                                        EM.Void_cannot_be_instantiated pos
                                        ILExpr.Error(ty)
                                    else
                                        ILExpr.Default(ty)
                                else
                                    match ty.GetConstructor(argTys) with
                                    | null -> 
                                        EM.Could_not_resolve_constructor pos ty.Name (args |> List.map(fun arg -> arg.Type) |> sprintTypes)
                                        ILExpr.Error(ty)
                                    | ctor ->
                                        ILExpr.Ctor(ctor, castArgsIfNeeded (ctor.GetParameters()) args, ty)
                    | NVT.Type(ty) -> //static methods of an open type
                        if ident.IsShort then
                            match tryResolveMethodWithGenericArgs env ty ident.ShortSuffix staticFlags (genericArgs |> List.toArray) argTys genericArgsPos with
                            | Some(meth) ->
                                ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
                            | None ->
                                loop tl
                        else
                            loop tl
            loop env.NVTs
        | Ast.SynExpr.GenericTypeStaticCall(tyName, (tyGenericArgs, genericArgsPos), methodName, methodGenericArgs, args, pos) -> //todo: need more position info for different tokens
            match tryResolveType env (Ast.TySig(tyName, tyGenericArgs)) with
            | None -> 
                EM.Could_not_resolve_type pos (Ast.TySig(tyName,tyGenericArgs).Name)
                abort()
            | Some(ty) ->
                let args = args |> List.map (tycheckExp)
                let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
                match tryResolveMethodWithGenericArgs env ty methodName staticFlags (methodGenericArgs |> List.toArray) argTys genericArgsPos with
                | None -> 
                    EM.Invalid_static_method pos methodName ty.Name (sprintTypes argTys)
                    abort()
                | Some(meth) -> 
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | Ast.SynExpr.ExpCall(instance, methodName, (methodGenericArgs, genericArgsPos), args, pos) ->
            let instance = tycheckExp instance
            let args = args |> List.map (tycheckExp)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            match tryResolveMethodWithGenericArgs env instance.Type methodName instanceFlags (methodGenericArgs |> List.toArray) argTys genericArgsPos with
            | None -> 
                EM.Invalid_instance_method pos methodName instance.Type.Name (sprintTypes argTys)
                abort()
            | Some(meth) ->
                ILExpr.InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | Ast.SynExpr.Let(name, (assign, assignPos), body) ->
            let assign = tycheckExp assign
            if assign.Type = typeof<Void> then
                EM.Void_invalid_in_let_binding assignPos
        
            let body = tycheckExpWith (env.ConsVariable(name, assign.Type)) body
            ILExpr.Let(name, assign, body, body.Type)
        | Ast.SynExpr.Var(ident, pos) ->
            match tryResolveVarFieldOrProperty env ident with
            | Some(VFP.FieldOrProperty(FP.Property(pi))) -> ILExpr.StaticCall(pi.GetGetMethod(), [], pi.PropertyType)
            | Some(VFP.FieldOrProperty(FP.Field(fi))) -> ILExpr.StaticFieldGet(fi)
            | Some(VFP.Var(name,ty)) -> ILExpr.Var(name,ty)
            | None ->
                EM.Variable_field_or_property_not_found pos ident.Full
                abort()
            
        | Ast.SynExpr.VarSet((ident, identPos), x, pos) ->
            let x = tycheckExp x

            match tryResolveVarFieldOrProperty env ident with
            | Some(vfp) ->
                let ilExpr, vfpTy =
                    match vfp with
                    | VFP.FieldOrProperty(FP.Property(pi)) -> ILExpr.StaticCall(pi.GetSetMethod(), [x], typeof<Void>), pi.PropertyType
                    | VFP.FieldOrProperty(FP.Field(fi)) -> ILExpr.StaticFieldSet(fi,x), fi.FieldType
                    | VFP.Var(name, varTy) -> ILExpr.VarSet(name,x), varTy
                
                if x.Type <> vfpTy then
                    EM.Variable_set_type_mismatch pos ident.Full ilExpr.Type.Name x.Type.Name
                    ILExpr.Error(typeof<Void>)
                else
                    ilExpr
            | None ->
                EM.Variable_field_or_property_not_found pos ident.Full
                ILExpr.Error(typeof<Void>)
        | Ast.SynExpr.Sequential((Ast.SynExpr.Break(_)|Ast.SynExpr.Continue(_)) as x, (_,pos)) ->
            EM.Unreachable_code_detected pos
            tycheckExp x //error recovery
        | Ast.SynExpr.Sequential(x,(y,_)) ->
            let x, y = tycheckExp x, tycheckExp y
            ILExpr.Sequential(x,y,y.Type)
        | Ast.SynExpr.OpenNamespaceOrType((ident, tySigs,pos), x) ->
            match tySigs with
            | [] when namespaceExists env.Assemblies ident.Full ->
                tycheckExpWith (env.ConsNamespace(ident.Full)) x
            | _ ->
                let tySig = TySig(ident, tySigs)
                match tryResolveType env tySig with
                | Some(ty) ->
                    tycheckExpWith (env.ConsType(ty)) x
                | None ->
                    EM.Namespace_or_type_not_found pos tySig.Name (sprintAssemblies env.Assemblies)
                    tycheckExp x
        | Ast.SynExpr.OpenAssembly((name,pos), x) ->
            let asm = tryLoadAssembly name
            match asm with
            | None -> 
                EM.Could_not_resolve_assembly pos name
                tycheckExp x
            | Some(asm) -> tycheckExpWith {env with Assemblies=asm::env.Assemblies} x
        | Ast.SynExpr.LogicalNot(x,pos) ->
            let x = tycheckExp x
            if x.Type <> typeof<bool> then
                EM.Expected_type_but_got_type pos "System.Boolean" x.Type.Name
                ILExpr.Error(typeof<bool>)
            else
                ILExpr.LogicalNot(x)
        | Ast.SynExpr.Cast(x, (ty, tyPos), pos) ->
            let x = tycheckExp x
            match tryResolveType env ty with
            | None -> 
                EM.Could_not_resolve_type tyPos ty.Name
                abort()
            | Some(ty) ->
                if ty = typeof<System.Void> then
                    EM.Casting_to_void_invalid pos
                    ILExpr.Error(ty)
                elif x.Type = ty then
                    EM.Casting_noop pos x.Type.Name
                    x
                elif ty.IsAssignableFrom(x.Type) || x.Type.IsAssignableFrom(ty) then
                    ILExpr.Cast(x,ty)
                elif (x.Type = typeof<int> && ty = typeof<float>) || (x.Type = typeof<float> && ty = typeof<int>) then
                    ILExpr.Coerce(x,ty) 
                else
                    let meth = seq {
                        //giver implicit conversion op from either type over explicit ops; next prefer conversion op defined on lhs type over rhs type
                        yield tryResolveOpImplicit ty x.Type ty
                        yield tryResolveOpImplicit x.Type x.Type ty
                        yield tryResolveOpExplicit ty x.Type ty
                        yield tryResolveOpExplicit x.Type x.Type ty } |> Seq.tryPick id

                    match meth with
                    | Some(meth) -> 
                        ILExpr.StaticCall(meth, [x], meth.ReturnType)    
                    | None -> 
                        EM.Casting_from_type_to_type_always_invalid pos x.Type.Name ty.Name
                        ILExpr.Error(ty)
        | Ast.SynExpr.LogicBinop(op,(x,xpos),(y,ypos)) ->
            let x =
                match tycheckExp x with
                | x when x.Type <> typeof<bool> -> 
                    EM.Expected_type_but_got_type xpos "System.Boolean" x.Type.Name
                    ILExpr.Error(typeof<bool>)
                | x -> x

            let y = 
                match tycheckExp y with
                | y when y.Type <> typeof<bool> ->
                    EM.Expected_type_but_got_type ypos "System.Boolean" y.Type.Name
                    ILExpr.Error(typeof<bool>)
                | y -> y
        
            match op with
            | Ast.SynLogicBinop.And -> ILExpr.IfThenElse(x, y, ILExpr.Bool(false), typeof<bool>)
            | Ast.SynLogicBinop.Or -> ILExpr.IfThenElse(x, ILExpr.Bool(true), y, typeof<bool>)
        | Ast.SynExpr.IfThenElse((condition, conditionPos),thenBranch,elseBranch,pos) ->
            let condition = 
                let condition = tycheckExp condition
                if condition.Type <> typeof<bool> then
                    EM.Expected_type_but_got_type conditionPos "System.Boolean" condition.Type.Name
                    ILExpr.Error(typeof<bool>)
                else
                    condition
            let thenBranch = tycheckExp thenBranch
            match elseBranch with
            | Some(elseBranch) ->
                let elseBranch = tycheckExp elseBranch
                if thenBranch.Type <> elseBranch.Type then
                    //maybe could use 
                    EM.IfThenElse_branch_type_mismatch pos thenBranch.Type.Name elseBranch.Type.Name
                    ILExpr.IfThenElse(condition,thenBranch, ILExpr.Error(thenBranch.Type), thenBranch.Type)
                else
                    ILExpr.IfThenElse(condition,thenBranch,elseBranch,thenBranch.Type)
            | None ->
                if thenBranch.Type = typeof<Void> then
                    ILExpr.IfThen(condition,thenBranch)
                else
                    ILExpr.IfThenElse(condition, thenBranch, ILExpr.Default(thenBranch.Type), thenBranch.Type)
        | Ast.SynExpr.Nop ->
            ILExpr.Nop
        | Ast.SynExpr.WhileLoop((condition, conditionPos), body) ->
            let condition = 
                let condition = tycheckExp condition
                if condition.Type <> typeof<bool> then
                    EM.Expected_type_but_got_type conditionPos "System.Boolean" condition.Type.Name
                    ILExpr.Error(typeof<bool>)
                else
                    condition
            let body = tycheckExpWith {env with IsLoopBody=true} body
            ILExpr.WhileLoop(condition, body)
        | Ast.SynExpr.Break(pos) ->
            if not env.IsLoopBody then
                EM.Break_outside_of_loop pos
                ILExpr.Error(typeof<Void>)
            else
                ILExpr.Break
        | Ast.SynExpr.Continue(pos) ->
            if not env.IsLoopBody then
                EM.Continue_outside_of_loop pos
                ILExpr.Error(typeof<Void>)
            else
                ILExpr.Continue

    match synTopLevel with
    | Ast.SynTopLevel.StmtList(xl) ->
        let rec loop env synStmts ilStmts =
            match synStmts with
            | [] -> ilStmts
            | synStmt::synStmts ->
                match synStmt with
                | Ast.SynStmt.Do x ->
                    let ilStmt = ILStmt.Do(tycheckExpWith env x)
                    loop env synStmts (ilStmt::ilStmts)                
                | Ast.SynStmt.Let(name, (assign,assignPos)) ->
                    let assign = tycheckExpWith env assign
                    if assign.Type = typeof<Void> then
                        EM.Void_invalid_in_let_binding assignPos

                    let ilStmt = ILStmt.Let(name, assign)
                    let env = env.ConsVariable(name, assign.Type)
                
                    loop env synStmts (ilStmt::ilStmts)
                | Ast.SynStmt.OpenAssembly(name, pos) ->
                    let asm = tryLoadAssembly name
                    match asm with
                    | None -> 
                        EM.Could_not_resolve_assembly pos name
                        loop env synStmts ilStmts //error recovery
                    | Some(asm) -> 
                        loop { env with Assemblies=asm::env.Assemblies } synStmts ilStmts
                | Ast.SynStmt.OpenNamespaceOrType(ident, tySigs, pos) ->
                    match tySigs with
                    | [] when namespaceExists env.Assemblies ident.Full ->
                        loop (env.ConsNamespace(ident.Full)) synStmts ilStmts
                    | _ ->
                        let tySig = TySig(ident,tySigs)
                        match tryResolveType env tySig with
                        | Some(ty) ->
                            loop (env.ConsType(ty)) synStmts ilStmts
                        | None ->
                            EM.Namespace_or_type_not_found pos tySig.Name (sprintAssemblies env.Assemblies)
                            loop env synStmts ilStmts //error recovery    
        ILTopLevel.StmtList(loop env xl [])
    | Ast.SynTopLevel.Expr(x) ->
        ILTopLevel.Exp(tycheckExpWith env x)