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

let sprintTySigs (tarr:TySig seq) =
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

let rec tryResolveType (namespaces:string seq) (assemblies: Assembly seq) name (tyArgs: Type seq) =
    namespaces
    |> Seq.collect (fun ns ->
        let possibleName = withNamespace name ns
        assemblies
        |> Seq.map (fun possibleAsm -> possibleName, possibleAsm))
    |> Seq.tryPick (fun (possibleName, possibleAsm) ->
        if tyArgs |> Seq.isEmpty then
            let possibleFullName = possibleName + ", " + possibleAsm.FullName
            Option.ofAllowsNull <| Type.GetType(possibleFullName,false, true)
        else
            let possibleFullName = 
                sprintf "%s`%i[%s], %s" 
                    possibleName 
                    (Seq.length tyArgs)
                    (String.concat "," (tyArgs |> Seq.map (fun ty -> sprintf "[%s]" ty.AssemblyQualifiedName)))
                    possibleAsm.FullName
            Option.ofAllowsNull <| Type.GetType(possibleFullName,false, true))
and tryResolveTySig (env:SemanticEnvironment) gsig =
    match gsig with
    | TySig(name, []) -> tryResolveType env.Namespaces env.Assemblies name []
    | TySig(name, args) ->  
        let args = args |> List.map (tryResolveTySig env)
        if args |> Seq.forall Option.isSome then
            tryResolveType env.Namespaces env.Assemblies name (args |> List.choose id)
        else
            None

let (|Resolved|NotResolved|) (env, tySigs) =
    let tySigResolutions = tySigs |> List.map (tryResolveTySig env)
    if tySigResolutions |> Seq.forall Option.isSome then
        Resolved(tySigResolutions |> List.choose id)
    else
        NotResolved((tySigs,tySigResolutions) 
                    ||> Seq.map2 
                        (fun tySig tySigResolution -> 
                            match tySigResolution with 
                            None -> Some(tySig) 
                            | _ -> None)
                    |> Seq.choose id 
                    |> Seq.toList)

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

let tryResolveVarFieldOrProperty env (path:Path) =
    let rec loop = function
        | [] -> None
        | hd::tl ->
            match hd with
            | NVT.Variable(name,ty) when name = path.Text -> //local var
                Some(VFP.Var(name,ty))
            | NVT.Namespace(ns) when path.IsLong ->
                match tryResolveType [ns] env.Assemblies path.LongPrefix [] with
                | Some(ty) ->
                    match tryResolveFieldOrProperty ty path.ShortSuffix staticFlags with
                    | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                    | None -> loop tl
                | None -> loop tl
            | NVT.Type(ty) when path.IsShort -> //field or property of an open type
                match tryResolveFieldOrProperty ty path.ShortSuffix staticFlags with
                | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                | None -> loop tl
            | _ -> loop tl
    loop env.NVTs

let tryResolvePath env (path:Path) =
    let rec loop = function
        | [] -> None
        | cur::rest ->
            match tryResolveVarFieldOrProperty env cur with
            | Some(vfp) ->
                match rest with
                | [] -> Some(vfp,None)
                | rest ->
                    Some(vfp, Some(Path.JoinShortSuffixes(rest)))
            | None -> loop rest
    loop path.Parts

let resolveTySigs env tySigs =
    match env, tySigs with
    | Resolved resolved -> resolved |> List.toArray
    | NotResolved notResolved ->
        for nr in notResolved do
            EM.Could_not_resolve_type nr.Pos nr.Name
        abort()

let resolveTySig env (tySig:TySig) =
    let tyTys = resolveTySigs env tySig.GenericArgs
    match tryResolveType env.Namespaces env.Assemblies tySig.GenericName tyTys with
    | Some(ty) -> ty
    | None -> 
        EM.Could_not_resolve_type tySig.Pos tySig.Name //todo: specific pos for ty name
        abort()

let resolveType namespaces assemblies name tyTys (originalTySig:TySig) =
    match tryResolveType namespaces assemblies name tyTys with
    | Some(ty) -> ty
    | None ->    
        EM.Could_not_resolve_type originalTySig.Pos originalTySig.Name //todo: specific pos for ty name
        abort()

///Symantic analysis (type checking)
let rec tycheckWith env synTopLevel =
    let rec tycheckExpWith env synExpr=
        let tycheckExp = tycheckExpWith env

        match synExpr with
        | SynExpr.Double x -> ILExpr.Double x
        | SynExpr.Int32 x  -> ILExpr.Int32 x
        | SynExpr.String x -> ILExpr.String x
        | SynExpr.Char x   -> ILExpr.Char x
        | SynExpr.Bool x   -> ILExpr.Bool x
        | SynExpr.Null(tySig) -> 
            let ty = resolveTySig env tySig
            if ty.IsValueType then
                EM.Null_is_invalid_for_value_types tySig.Pos ty.Name
                ILExpr.Null(ty) //error recovery: use wants to use a ValueType, but incorrectly wanted to use null for it
            else
                ILExpr.Null(ty)
        | SynExpr.Typeof(tySig)   -> 
            match tryResolveTySig env tySig with
            | None -> 
                EM.Could_not_resolve_type tySig.Pos tySig.Name
                ILExpr.Typeof(typeof<obj>) //error recovery: this is a runtime value that won't hurt us error 
            | Some(ty) -> 
                ILExpr.Typeof(ty)
        | SynExpr.Default(tySig)   -> 
            let ty = resolveTySig env tySig
            if ty = typeof<System.Void> then
                EM.Void_cannot_be_instantiated tySig.Pos

                ILExpr.Default(ty) //error recovery
            else
                ILExpr.Default(ty)
        | SynExpr.UMinus(x,pos) ->
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
        | SynExpr.Pow(x, y, pos) ->        
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
        | SynExpr.NumericBinop(op,x,y,pos) ->
            let x, y = tycheckExp x, tycheckExp y
            match NumericTower.tallestTy x.Type y.Type with
            | Some(tallestTy) -> //primitive
                ILExpr.mkNumericBinop(op, coerceIfNeeded tallestTy x, coerceIfNeeded tallestTy y, tallestTy)
            | None when op = SynNumericBinop.Plus && (x.Type = typeof<string> || y.Type = typeof<string>) -> //string
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
        | SynExpr.ComparisonBinop(op, x, y, pos) ->
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
                | None, (SynComparisonBinop.Eq | SynComparisonBinop.Neq) when (x.Type.IsAssignableFrom(y.Type) || y.Type.IsAssignableFrom(x.Type)) && (not (x.Type.IsValueType <> y.Type.IsValueType)) -> 
                    ILExpr.mkComparisonBinop(op, x, y)    
                | None, _ ->
                    EM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                    ILExpr.Error(typeof<bool>)
        //this is our most complex part of the grammer...
        | SynExpr.PathCall(path, genericTyArgs, args, pos) ->
            let args = args |> List.map (tycheckExp)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            let genericTyArgs = resolveTySigs env genericTyArgs

            let rec loop = function
                | [] -> 
                    EM.Could_not_resolve_possible_method_call_or_contructor_type pos path.LongPrefix path.Text //TODO: may not be accurate any more!
                    abort() //need error message too!
                | hd::tl ->
                    match hd with
                    | NVT.Variable(name,ty) when name = path.LongPrefix -> //instance method call on variable
                        match tryResolveMethod ty path.ShortSuffix instanceFlags genericTyArgs argTys with
                        | None -> 
                            EM.Invalid_instance_method pos path.ShortSuffix ty.Name (sprintTypes argTys)
                            abort()
                        | Some(meth) -> 
                            ILExpr.InstanceCall(ILExpr.VarGet(path.LongPrefix,ty), meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
                    | NVT.Namespace(ns) ->
                        match tryResolveType [ns] env.Assemblies path.LongPrefix [] with
                        | Some(ty) -> //static method call (possibly generic) on non-generic type (need to handle generic type in another parse case, i think)
                            match tryResolveMethod ty path.ShortSuffix staticFlags genericTyArgs argTys with
                            | None -> 
                                EM.Invalid_static_method pos path.ShortSuffix ty.Name (sprintTypes argTys)
                                abort()
                            | Some(meth) -> 
                                ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
                        | None -> //constructors (generic or non-generic)
                            match tryResolveType [ns] env.Assemblies path.Text genericTyArgs with
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
                    | NVT.Type(ty) when path.IsShort -> //static methods of an open type
                        match tryResolveMethod ty path.ShortSuffix staticFlags genericTyArgs argTys with
                        | Some(meth) ->
                            ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
                        | None ->
                            loop tl
                    | _ ->
                        loop tl
            loop env.NVTs
        | SynExpr.GenericTypeStaticCall(tyName, tyGenericTyArgs, methodName, methodGenericTyArgs, args, pos) -> //todo: need more position info for different tokens
            let tyGenericTyArgs = resolveTySigs env tyGenericTyArgs
            let methodGenericTyArgs = resolveTySigs env methodGenericTyArgs

            match tryResolveType env.Namespaces env.Assemblies tyName tyGenericTyArgs with
            | None -> 
                EM.Could_not_resolve_type pos tyName //TODO: IMPROVE IDENT POS INFO
                abort()
            | Some(ty) ->
                let args = args |> List.map (tycheckExp)
                let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
                match tryResolveMethod ty methodName staticFlags methodGenericTyArgs argTys with
                | None -> 
                    EM.Invalid_static_method pos methodName ty.Name (sprintTypes argTys)
                    abort()
                | Some(meth) -> 
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | SynExpr.ExprPathCall(instance, path, methodGenericTyArgs, args, pos) ->
//            let instance =                
//                if path.IsLong then
//                    SynExpr.exp
            let methodName = path.Text
            let instance = tycheckExp instance
            let args = args |> List.map (tycheckExp)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            let methodGenericTyArgs = resolveTySigs env methodGenericTyArgs

            match tryResolveMethod instance.Type methodName instanceFlags methodGenericTyArgs argTys with
            | None -> 
                EM.Invalid_instance_method pos methodName instance.Type.Name (sprintTypes argTys)
                abort()
            | Some(meth) ->
            ILExpr.InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args, meth.ReturnType)
        | SynExpr.Let(name, (assign, assignPos), body) ->
            let assign = tycheckExp assign
            if assign.Type = typeof<Void> then
                EM.Void_invalid_in_let_binding assignPos
        
            let body = tycheckExpWith (env.ConsVariable(name, assign.Type)) body
            ILExpr.Let(name, assign, body, body.Type)
        | SynExpr.PathGet(path, pos) ->
            match tryResolveVarFieldOrProperty env path with
            | Some(vfp) ->
                match vfp with
                | VFP.FieldOrProperty(FP.Property(pi)) -> ILExpr.StaticCall(pi.GetGetMethod(), [], pi.PropertyType)
                | VFP.FieldOrProperty(FP.Field(fi)) -> ILExpr.StaticFieldGet(fi)
                | VFP.Var(name,ty) -> ILExpr.VarGet(name,ty)
            | None ->
                EM.Variable_field_or_property_not_found pos path.Text
                abort()
            


//            match tryResolvePath env path with
//            | None ->
//                EM.Variable_field_or_property_not_found pos path.Text
//                abort()
//            | Some(vfp, path) ->
//                let ilx =
//                    match vfp with
//                    | VFP.FieldOrProperty(FP.Property(pi)) -> ILExpr.StaticCall(pi.GetGetMethod(), [], pi.PropertyType)
//                    | VFP.FieldOrProperty(FP.Field(fi)) -> ILExpr.StaticFieldGet(fi)
//                    | VFP.Var(name,ty) -> ILExpr.Var(name,ty)
//                
//                match path with
//                | None -> ilx
//                | Some(path) -> ilx //TODO
        | SynExpr.PathSet((path, pathPos), x, pos) ->
            let x = tycheckExp x

            match tryResolveVarFieldOrProperty env path with
            | Some(vfp) ->
                let ilExpr, vfpTy =
                    match vfp with
                    | VFP.FieldOrProperty(FP.Property(pi)) -> ILExpr.StaticCall(pi.GetSetMethod(), [x], typeof<Void>), pi.PropertyType
                    | VFP.FieldOrProperty(FP.Field(fi)) -> ILExpr.StaticFieldSet(fi,x), fi.FieldType
                    | VFP.Var(name, varTy) -> ILExpr.VarSet(name,x), varTy
                
                if x.Type <> vfpTy then
                    EM.Variable_set_type_mismatch pos path.Text ilExpr.Type.Name x.Type.Name
                    ILExpr.Error(typeof<Void>)
                else
                    ilExpr
            | None ->
                EM.Variable_field_or_property_not_found pos path.Text
                ILExpr.Error(typeof<Void>)
        | SynExpr.Sequential((SynExpr.Break(_)|SynExpr.Continue(_)) as x, (_,pos)) ->
            EM.Unreachable_code_detected pos
            tycheckExp x //error recovery
        | SynExpr.Sequential(x,(y,_)) ->
            let x, y = tycheckExp x, tycheckExp y
            ILExpr.Sequential(x,y,y.Type)
        | SynExpr.OpenNamespaceOrType(nsOrTy, x) ->
            match nsOrTy.GenericArgs with
            | [] when namespaceExists env.Assemblies nsOrTy.GenericName ->
                tycheckExpWith (env.ConsNamespace(nsOrTy.GenericName)) x
            | _ ->
                let tyTys = resolveTySigs env nsOrTy.GenericArgs
                match tryResolveType env.Namespaces env.Assemblies nsOrTy.GenericName tyTys with
                | Some(ty) ->
                    tycheckExpWith (env.ConsType(ty)) x
                | None ->
                    EM.Namespace_or_type_not_found nsOrTy.Pos nsOrTy.Name (sprintAssemblies env.Assemblies)
                    tycheckExp x
        | SynExpr.OpenAssembly((name,pos), x) ->
            let asm = tryLoadAssembly name
            match asm with
            | None -> 
                EM.Could_not_resolve_assembly pos name
                tycheckExp x
            | Some(asm) -> tycheckExpWith {env with Assemblies=asm::env.Assemblies} x
        | SynExpr.LogicalNot(x,pos) ->
            let x = tycheckExp x
            if x.Type <> typeof<bool> then
                EM.Expected_type_but_got_type pos "System.Boolean" x.Type.Name
                ILExpr.Error(typeof<bool>)
            else
                ILExpr.LogicalNot(x)
        | SynExpr.Cast(x, tySig, pos) ->
            let x = tycheckExp x
            let ty = resolveTySig env tySig
            
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
        | SynExpr.LogicBinop(op,(x,xpos),(y,ypos)) ->
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
            | SynLogicBinop.And -> ILExpr.IfThenElse(x, y, ILExpr.Bool(false), typeof<bool>)
            | SynLogicBinop.Or -> ILExpr.IfThenElse(x, ILExpr.Bool(true), y, typeof<bool>)
        | SynExpr.IfThenElse((condition, conditionPos),thenBranch,elseBranch,pos) ->
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
        | SynExpr.Nop ->
            ILExpr.Nop
        | SynExpr.WhileLoop((condition, conditionPos), body) ->
            let condition = 
                let condition = tycheckExp condition
                if condition.Type <> typeof<bool> then
                    EM.Expected_type_but_got_type conditionPos "System.Boolean" condition.Type.Name
                    ILExpr.Error(typeof<bool>)
                else
                    condition
            let body = tycheckExpWith {env with IsLoopBody=true} body
            ILExpr.WhileLoop(condition, body)
        | SynExpr.Break(pos) ->
            if not env.IsLoopBody then
                EM.Break_outside_of_loop pos
                ILExpr.Error(typeof<Void>)
            else
                ILExpr.Break
        | SynExpr.Continue(pos) ->
            if not env.IsLoopBody then
                EM.Continue_outside_of_loop pos
                ILExpr.Error(typeof<Void>)
            else
                ILExpr.Continue

    match synTopLevel with
    | SynTopLevel.StmtList(xl) ->
        let rec loop env synStmts ilStmts =
            match synStmts with
            | [] -> ilStmts
            | synStmt::synStmts ->
                match synStmt with
                | SynStmt.Do x ->
                    let ilStmt = ILStmt.Do(tycheckExpWith env x)
                    loop env synStmts (ilStmt::ilStmts)                
                | SynStmt.Let(name, (assign,assignPos)) ->
                    let assign = tycheckExpWith env assign
                    if assign.Type = typeof<Void> then
                        EM.Void_invalid_in_let_binding assignPos

                    let ilStmt = ILStmt.Let(name, assign)
                    let env = env.ConsVariable(name, assign.Type)
                
                    loop env synStmts (ilStmt::ilStmts)
                | SynStmt.OpenAssembly(name, pos) ->
                    let asm = tryLoadAssembly name
                    match asm with
                    | None -> 
                        EM.Could_not_resolve_assembly pos name
                        loop env synStmts ilStmts //error recovery
                    | Some(asm) -> 
                        loop { env with Assemblies=asm::env.Assemblies } synStmts ilStmts
                | SynStmt.OpenNamespaceOrType(nsOrTy) ->
                    match nsOrTy.GenericArgs with
                    | [] when namespaceExists env.Assemblies nsOrTy.GenericName ->
                        loop (env.ConsNamespace(nsOrTy.GenericName)) synStmts ilStmts
                    | _ ->
                        let tyTys = resolveTySigs env nsOrTy.GenericArgs
                        match tryResolveType env.Namespaces env.Assemblies nsOrTy.GenericName tyTys with
                        | Some(ty) ->
                            loop (env.ConsType(ty)) synStmts ilStmts
                        | None ->
                            EM.Namespace_or_type_not_found nsOrTy.Pos nsOrTy.Name (sprintAssemblies env.Assemblies)
                            loop env synStmts ilStmts //error recovery
        ILTopLevel.StmtList(loop env xl [])
    | SynTopLevel.Expr(x) ->
        ILTopLevel.Exp(tycheckExpWith env x)