module Swensen.NL.SemanticAnalysis

open System
open System.Reflection
open Swensen.NL.Ast
open Swensen.NL.Ail

module EM = ErrorMessages
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

let coerceIfNeeded cked targetTy (sourceExp:ILExpr) =
    if sourceExp.Type <> targetTy then Coerce(cked, sourceExp,targetTy) else sourceExp

let castIfNeeded targetTy (sourceExp:ILExpr) =
    if sourceExp.Type <> targetTy then Cast(sourceExp,targetTy) else sourceExp

///SHOULD MAKE A "CAST OR COERCE IF NEEDED" METHOD!! BUG 31 
let castArgsIfNeeded (targetParameterInfo:ParameterInfo[]) sourceArgExps =
    List.zip (targetParameterInfo |> Seq.map (fun p -> p.ParameterType) |> Seq.toList)  sourceArgExps
    |> List.map (fun (targetTy, sourceExp) -> castIfNeeded targetTy sourceExp)

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
        | _ -> None //i.e. ambiguous

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
            let asm = Assembly.LoadFrom(name)
            //if asm is not being referenced by the currently executing assembly, then
            //it will fail to load by assembly qualified name (i.e. Type.GetType in tryResolve type),
            //so we need to add an assembly resolve handler here.
            //TODO: look into what happens when we try to resolve the same assembly (by dll name, by assembly name...?)
            //multiple times.  possible have asm.FullName / handler map where we remove the old handler and add a new one,
            //supposing that we could load multiple handlers).
            AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun sender are -> 
                if are.Name = asm.FullName then asm else null))
            Some(asm)
        with _ ->
            None

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

//let resolveType namespaces assemblies name tyTys (originalTySig:TySig) =
//    match tryResolveType namespaces assemblies name tyTys with
//    | Some(ty) -> ty
//    | None ->    
//        EM.Could_not_resolve_type originalTySig.Pos originalTySig.Name //todo: specific pos for ty name
//        abort()

let resolveILExprStaticCall ty methodName methodGenericTyArgs args argTys pos =
    match tryResolveMethod ty methodName staticFlags methodGenericTyArgs argTys with
    | None -> 
        EM.Invalid_static_method pos methodName ty.Name (sprintTypes argTys)
        abort()
    | Some(meth) -> 
        ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args)

let resolveILExprInstanceCall (instance:ILExpr) (methodName:Path) methodGenericTyArgs args argTys =
    match tryResolveMethod instance.Type methodName.Text instanceFlags methodGenericTyArgs argTys with
    | None -> 
        EM.Invalid_instance_method methodName.Pos methodName.Text instance.Type.Name (sprintTypes argTys)
        abort()
    | Some(meth) -> 
        ILExpr.InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args)    

module PathResolution =
    type FP =
        | Field of FieldInfo //N.B. CONSTANT FIELDS LIKE INT32.MAXVALUE CANNOT BE ACCESSED NORMALLY: CHECK FOR ISLITERAL FIELD ATTRIBUTE AND EMIT LITERAL VALUE (CAN USE GETVALUE REFLECTION) -- IF CAN'T DO THAT, CHECK FIELDINFO HANDLE AND IF THROWS WE KNOW WE NEED TO STOP
        | Property of PropertyInfo

    let tryResolveFieldOrProperty ty name bindingFlags =
        match tryResolveField ty name bindingFlags with
        | Some(fi) -> Some(FP.Field(fi))
        | None ->
            match tryResolveProperty ty name bindingFlags with
            | Some(pi) -> Some(FP.Property(pi))
            | None -> None

    type VFP =
        | Var of string * Type
        | FieldOrProperty of FP

    let tryResolveStaticVarFieldOrProperty env (path:Path) =
        env.NVTs
        |> Seq.tryPick(fun nvt ->
            match nvt with
            | NVT.Variable(name,ty) when name = path.Text -> //local var
                Some(VFP.Var(name,ty))
            | NVT.Namespace(ns) when path.IsMultiPart ->
                match tryResolveType [ns] env.Assemblies path.LeadingPartsText [] with
                | Some(ty) ->
                    match tryResolveFieldOrProperty ty path.LastPartText staticFlags with
                    | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                    | None -> None
                | None -> None
            | NVT.Type(ty) when path.IsSinglePart -> //field or property of an open type
                match tryResolveFieldOrProperty ty path.LastPartText staticFlags with
                | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                | None -> None
            | _ -> None)

    let tryResolveILExprStaticFieldOrPropertyGet ty name rest =
        match tryResolveField ty name staticFlags with
        | Some(fi) -> Some(ILExpr.mkStaticFieldGet(fi), rest)
        | None ->
            match tryResolveMethod ty ("get_" + name) staticFlags [||] [||] with
            | Some(mi) -> Some(ILExpr.StaticCall(mi,[]),rest)
            | None -> None

    let tryResolveLeadingPathGet env (path:Path) =
        path.Expansion
        |> Seq.collect (fun p ->
            env.NVTs 
            |> Seq.map (fun nvt -> nvt, p))
        |> Seq.tryPick (fun (nvt, (path, rest)) ->
            match nvt with
            //a local variable
            | NVT.Variable(name,ty) when name = path.Text -> //local var
                Some(ILExpr.VarGet(name,ty), rest)
            //field or property of type resolved against an open namespace
            | NVT.Namespace(ns) when path.IsMultiPart ->
                match tryResolveType [ns] env.Assemblies path.LeadingPartsText [] with
                | Some(ty) -> tryResolveILExprStaticFieldOrPropertyGet ty path.LastPartText rest
                | None -> None
            //field or property of an open type
            | NVT.Type(ty) when path.IsSinglePart->
                tryResolveILExprStaticFieldOrPropertyGet ty path.LastPartText rest
            | _ -> None) 

    let rec tryResolveILExprInstancePathGet (ilExpr:ILExpr) (path:Path) =
        let path,rest = path.FirstPartPathWithRest
        let ilExpr =
            match tryResolveField ilExpr.Type path.Text instanceFlags with
            | Some(fi) -> Some(ILExpr.InstanceFieldGet(ilExpr,fi))
            | None ->
                match tryResolveMethod ilExpr.Type ("get_" + path.Text) instanceFlags [||] [||] with
                | Some(mi) -> Some(ILExpr.InstanceCall(ilExpr,mi,[]))
                | None -> 
                    None

        match ilExpr, rest with
        | Some(ilExpr), Some(rest) -> tryResolveILExprInstancePathGet ilExpr rest
        | Some(ilExpr), None -> Some(ilExpr)
        | None, _ -> None

    //the reason we don't just implement this in terms of tryResolveILExprInstancePathGet is because we 
    //want the precise point of ailure in the rest chain (of course, we could implement tryResolveILExprInstancePathGet
    //with more info returned...)
    let rec resolveILExprInstancePathGet (ilExpr:ILExpr) (path:Path) =
        let path,rest = path.FirstPartPathWithRest
        let ilExpr =
            match tryResolveField ilExpr.Type path.Text instanceFlags with
            | Some(fi) -> ILExpr.InstanceFieldGet(ilExpr,fi)
            | None ->
                match tryResolveMethod ilExpr.Type ("get_" + path.Text) instanceFlags [||] [||] with
                | Some(mi) -> ILExpr.InstanceCall(ilExpr,mi,[])
                | None -> 
                    EM.Instance_field_or_property_not_found path.Pos path.Text ilExpr.Type.Name
                    abort()

        match rest with
        | Some(rest) -> resolveILExprInstancePathGet ilExpr rest
        | None -> ilExpr

    let validateFieldSet (fi:FieldInfo) (path:Path) (assignTy:Type) (assignPos) (ifValid:Lazy<_>) =
        if not <| fi.FieldType.IsAssignableFrom(assignTy) then //allow implicit cast?
            EM.Field_set_type_mismatch (PositionRange(path.Pos,assignPos)) path.Text fi.FieldType.Name assignTy.Name
            ILExpr.Error(typeof<Void>)
        else
            ifValid.Value

    let validatePropertySet (pi:PropertyInfo) (path:Path) (assignTy:Type) (assignPos) (ifValid:Lazy<_>) =
        if not pi.CanWrite then
            EM.Property_has_no_setter path.Pos path.Text
            ILExpr.Error(typeof<Void>)
        else
            if not <| pi.PropertyType.IsAssignableFrom(assignTy) then //allow implicit cast?
                EM.Property_set_type_mismatch (PositionRange(path.Pos, assignPos)) path.Text pi.PropertyType.Name assignTy.Name
                ILExpr.Error(typeof<Void>)
            else 
                ifValid.Value

    let resolveILExprInstancePathSet (instance:ILExpr) (path:Path) (assign:ILExpr) (assignPos) =
        match tryResolveFieldOrProperty instance.Type path.Text instanceFlags with
        | Some(FP.Field(fi)) ->
            validateFieldSet fi path assign.Type assignPos
                (lazy(ILExpr.InstanceFieldSet(instance, fi, castIfNeeded fi.FieldType assign)))
        | Some(FP.Property(pi)) ->
            validatePropertySet pi path assign.Type assignPos
                (lazy(ILExpr.InstancePropertySet(instance, pi, castIfNeeded pi.PropertyType assign)))
        | None ->
            EM.Instance_field_or_property_not_found path.Pos path.Text instance.Type.Name
            ILExpr.Error(typeof<Void>)

    ///resolves the full path call not allowing any leading chaining
    let resolveFullPathCall env (path:Path) genericTyArgs args argTys pos =
        let attempt =
            env.NVTs
            |> Seq.tryPick (function
                | NVT.Variable(name,ty) when name = path.LeadingPartsText -> //instance method call on variable
                    let instance = ILExpr.VarGet(path.LeadingPartsText,ty)
                    let methodName = path.LastPartPath
                    Some(resolveILExprInstanceCall instance methodName genericTyArgs args argTys)
                | NVT.Namespace(ns) ->
                    match tryResolveType [ns] env.Assemblies path.LeadingPartsText [] with
                    | Some(ty) -> //static method call (possibly generic) on non-generic type (need to handle generic type in another parse case, i think)
                        Some(resolveILExprStaticCall ty path.LastPartText genericTyArgs args argTys pos)
                    | None -> //constructors (generic or non-generic)
                        match tryResolveType [ns] env.Assemblies path.Text genericTyArgs with
                        | None -> 
                            None
                        | Some(ty) ->
                            if ty.IsValueType && args.Length = 0 then
                                if ty = typeof<System.Void> then
                                    EM.Void_cannot_be_instantiated pos
                                    Some(ILExpr.Error(ty))
                                else
                                    Some(ILExpr.Default(ty))
                            else
                                match ty.GetConstructor(argTys) with
                                | null -> 
                                    EM.Could_not_resolve_constructor pos ty.Name (args |> List.map(fun arg -> arg.Type) |> sprintTypes)
                                    Some(ILExpr.Error(ty))
                                | ctor ->
                                    Some(ILExpr.Ctor(ctor, castArgsIfNeeded (ctor.GetParameters()) args, ty))
                | NVT.Type(ty) when path.IsSinglePart -> //static methods of an open type
                    match tryResolveMethod ty path.LastPartText staticFlags genericTyArgs argTys with
                    | Some(meth) ->
                        Some(ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args))
                    | None ->
                        None
                | _ ->
                    None)
                
        match attempt with
        | None ->
            EM.Could_not_resolve_possible_method_call_or_contructor_type pos path.LeadingPartsText path.Text //TODO: may not be accurate any more!
            abort() //need error message too!
        | Some(ilExpr) ->
            ilExpr

module PR = PathResolution

//we need to do numeric parsing here instead of in the lexer so that the grammer handles uminus vs. minus correctly
module NumericParsing =
    let parseSByte input pos =
        match System.SByte.TryParse(input) with
        | true, value -> ILExpr.SByte(value)
        | _ ->
            EM.SByte_literal_out_of_range pos input
            ILExpr.SByte(0y) //error recovery

    let parseByte input pos =        
        match System.Byte.TryParse(input) with
        | true, value -> ILExpr.Byte(value)
        | _ ->
            EM.Byte_literal_out_of_range pos input
            ILExpr.Byte(0uy) //error recovery

    let parseInt16 input pos =        
        match System.Int16.TryParse(input) with
        | true, value -> ILExpr.Int16(value)
        | _ ->
            EM.Int16_literal_out_of_range pos input
            ILExpr.Int16(0s) //error recovery

    let parseInt32 input pos =        
        match System.Int32.TryParse(input) with
        | true, value -> ILExpr.Int32(value)
        | _ ->
            EM.Int32_literal_out_of_range pos input
            ILExpr.Int32(0) //error recovery

    let parseInt64 input pos =        
        match System.Int64.TryParse(input) with
        | true, value -> ILExpr.Int64(value)
        | _ ->
            EM.Int64_literal_out_of_range pos input
            ILExpr.Int64(0L) //error recovery

    let parseUInt16 input pos =        
        match System.UInt16.TryParse(input) with
        | true, value -> ILExpr.UInt16(value)
        | _ ->
            EM.UInt16_literal_out_of_range pos input
            ILExpr.UInt16(0us) //error recovery

    let parseUInt32 input pos =        
        match System.UInt32.TryParse(input) with
        | true, value -> ILExpr.UInt32(value)
        | _ ->
            EM.UInt32_literal_out_of_range pos input
            ILExpr.UInt32(0u) //error recovery

    let parseUInt64 input pos =        
        match System.UInt64.TryParse(input) with
        | true, value -> ILExpr.UInt64(value)
        | _ ->
            EM.UInt64_literal_out_of_range pos input
            ILExpr.UInt64(0UL) //error recovery

    let parseSingle input pos =        
        match System.Single.TryParse(input) with
        | true, value -> ILExpr.Single(value)
        | _ ->
            EM.Single_literal_out_of_range pos input
            ILExpr.Single(0.0f) //error recovery

    let parseDouble input pos =        
        match System.Double.TryParse(input) with
        | true, value -> ILExpr.Double(value)
        | _ ->
            EM.Double_literal_out_of_range pos input
            ILExpr.Double(0.0) //error recovery

///Symantic analysis (type checking)
let rec semantWith env synTopLevel =
    let rec semantExprWith env synExpr=
        let semantExpr = semantExprWith env

        match synExpr with
        | SynExpr.Byte(x,pos)   -> NumericParsing.parseByte x pos
        | SynExpr.SByte(x,pos)  -> NumericParsing.parseSByte x pos

        | SynExpr.Int16(x,pos)  -> NumericParsing.parseInt16 x pos
        | SynExpr.Int32(x,pos)  -> NumericParsing.parseInt32 x pos
        | SynExpr.Int64(x,pos)  -> NumericParsing.parseInt64 x pos
        
        | SynExpr.UInt16(x,pos) -> NumericParsing.parseUInt16 x pos
        | SynExpr.UInt32(x,pos) -> NumericParsing.parseUInt32 x pos
        | SynExpr.UInt64(x,pos) -> NumericParsing.parseUInt64 x pos
        
        | SynExpr.Single(x,pos) -> NumericParsing.parseSingle x pos
        | SynExpr.Double(x,pos) -> NumericParsing.parseDouble x pos

        | SynExpr.UMinus(SynExpr.Byte(x,xpos),upos)   -> NumericParsing.parseByte ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.SByte(x,xpos),upos)  -> NumericParsing.parseSByte ("-"+x) (PositionRange(upos,xpos))

        | SynExpr.UMinus(SynExpr.Int16(x,xpos),upos)  -> NumericParsing.parseInt16 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Int32(x,xpos),upos)  -> NumericParsing.parseInt32 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Int64(x,xpos),upos)  -> NumericParsing.parseInt64 ("-"+x) (PositionRange(upos,xpos))
        
        | SynExpr.UMinus(SynExpr.UInt16(x,xpos),upos) -> NumericParsing.parseUInt16 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.UInt32(x,xpos),upos) -> NumericParsing.parseUInt32 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.UInt64(x,xpos),upos) -> NumericParsing.parseUInt64 ("-"+x) (PositionRange(upos,xpos))
        
        | SynExpr.UMinus(SynExpr.Single(x,xpos),upos) -> NumericParsing.parseSingle ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Double(x,xpos),upos) -> NumericParsing.parseDouble ("-"+x) (PositionRange(upos,xpos))
        
        | SynExpr.String(x) -> ILExpr.String(x)
        | SynExpr.Char(x)   -> ILExpr.Char(x)
        | SynExpr.Bool(x)   -> ILExpr.Bool(x)

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
            let x = semantExpr x
            if NumericPrimitive.isNumericPrimitive x.Type then
               ILExpr.UMinus(env.Checked, x, x.Type)
            else           
                match x.Type.GetMethod("op_UnaryNegation") with
                | null ->
                    EM.No_overload_found_for_unary_operator pos "-" x.Type.Name
                    ILExpr.Error(x.Type)
                | meth ->
                    ILExpr.StaticCall(meth, [x])
        | SynExpr.Pow(x, y, pos) ->        
            let x,y = semantExpr x, semantExpr y
            //TODO: hmm, revisit this, i'm not so sure we want to pass in static types instead of true types of x and y, we know this should resolve
            match tryResolveMethod typeof<System.Math> "Pow" staticFlags [||] [|typeof<float>;typeof<float>|] with
            | None -> 
                EM.Internal_error pos "Failed to resolve 'System.Math.Pow(float,float)' for synthetic operator '**'"
                ILExpr.Error(typeof<float>)
            | Some(meth) ->
                if NumericPrimitive.sourceIsEqualOrHasImplicitConvToTarget x.Type typeof<Double> 
                    && NumericPrimitive.sourceIsEqualOrHasImplicitConvToTarget y.Type typeof<Double> then
                    ILExpr.StaticCall(meth, [x;y] |> List.map (coerceIfNeeded env.Checked typeof<Double>))
                else
                    EM.No_overload_found_for_binary_operator pos "**" x.Type.Name y.Type.Name
                    ILExpr.Error(typeof<float>)
        | SynExpr.NumericBinop(op,x,y,pos) ->
            let x, y = semantExpr x, semantExpr y
            match NumericPrimitive.tryGetEqualOrImplicitConvTarget x.Type y.Type with
            | Some(targetTy) -> //primitive
                ILExpr.mkNumericBinop(env.Checked, op, coerceIfNeeded env.Checked targetTy x, coerceIfNeeded env.Checked targetTy y, targetTy)
            | None when op = SynNumericBinop.Plus && (x.Type = typeof<string> || y.Type = typeof<string>) -> //string
                let meth = tryResolveMethod typeof<System.String> "Concat" staticFlags [||] [|x.Type; y.Type|]
                match meth with
                | None ->
                    //there should always be a String.Concat(obj,obj) overload
                    EM.Internal_error pos (sprintf "Could not resolve 'String.Concat' synthetic '+' overload for argument types %s" (sprintTypes [x.Type; y.Type]))
                    ILExpr.Error(typeof<string>) //error recovery
                | Some(meth) ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y])
            | None -> //static "op_*" overloads
                let meth = seq {
                    yield tryResolveMethod x.Type op.Name staticFlags [||] [|x.Type; y.Type|]
                    yield tryResolveMethod y.Type op.Name staticFlags [||] [|x.Type; y.Type|] } |> Seq.tryPick id

                match meth with
                | None ->
                    EM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                    ILExpr.Error(x.Type) //error recovery: best guess of intended return type
                | Some(meth) ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y]) 
        | SynExpr.ComparisonBinop(op, x, y, pos) ->
            let x, y = semantExpr x, semantExpr y
                        
            //first numeric tower value type cases
            match NumericPrimitive.tryGetEqualOrImplicitConvTarget x.Type y.Type with
            | Some(targetTy) ->
                ILExpr.mkComparisonBinop(op, coerceIfNeeded env.Checked targetTy x, coerceIfNeeded env.Checked targetTy y)
            | None ->
                //next operator overloads
                let meth = seq {
                    yield tryResolveMethod x.Type op.Name staticFlags [||] [|x.Type; y.Type|]
                    yield tryResolveMethod y.Type op.Name staticFlags [||] [|x.Type; y.Type|] } |> Seq.tryPick id

                match meth, op with
                | Some(meth), _ ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y])    
                //reference equals or non-numeric primitive comparison between bools or chars
                | None, (SynComparisonBinop.Eq | SynComparisonBinop.Neq) when (x.Type.IsAssignableFrom(y.Type) || y.Type.IsAssignableFrom(x.Type)) && (not (x.Type.IsValueType <> y.Type.IsValueType)) -> 
                    ILExpr.mkComparisonBinop(op, x, y)    
                | None, _ ->
                    EM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                    ILExpr.Error(typeof<bool>)
        | SynExpr.GenericTypeStaticCall(tyName, tyGenericTyArgs, methodName, methodGenericTyArgs, args, pos) -> //todo: need more position info for different tokens
            let tyGenericTyArgs = resolveTySigs env tyGenericTyArgs
            let methodGenericTyArgs = resolveTySigs env methodGenericTyArgs

            match tryResolveType env.Namespaces env.Assemblies tyName tyGenericTyArgs with
            | None -> 
                EM.Could_not_resolve_type pos tyName //TODO: IMPROVE IDENT POS INFO
                abort()
            | Some(ty) ->
                let args = args |> List.map (semantExpr)
                let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
                resolveILExprStaticCall ty methodName methodGenericTyArgs args argTys pos
        //this is our most complex part of the grammer...
        | SynExpr.PathCall(path, methodGenericTyArgs, args, pos) ->
            let args = args |> List.map (semantExpr)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            let methodGenericTyArgs = resolveTySigs env methodGenericTyArgs

            let instance =
                match path.LeadingPartsPath with
                | Some(path) -> 
                    match PR.tryResolveLeadingPathGet env path with
                    | None ->
                        None
                    | Some(ilExpr, Some(rest)) -> 
                        PR.tryResolveILExprInstancePathGet ilExpr rest
                    | Some(ilExpr, None) ->
                        Some(ilExpr)
                | None -> None

            match instance with
            | Some(instance) ->
                let methodName = path.LastPartPath
                resolveILExprInstanceCall instance methodName methodGenericTyArgs args argTys
            | None ->
                PR.resolveFullPathCall env path methodGenericTyArgs args argTys pos
        | SynExpr.ExprPathCall(instance, path, methodGenericTyArgs, args, pos) -> //DONE
            let instance, methodName =
                match path.LeadingPartsPath with
                | Some(leadingPath) ->
                    semantExpr <| SynExpr.ExprPathGet(instance, leadingPath), path.LastPartPath
                | None ->
                    semantExpr instance, path

            let args = args |> List.map (semantExpr)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            let methodGenericTyArgs = resolveTySigs env methodGenericTyArgs

            resolveILExprInstanceCall instance methodName methodGenericTyArgs args argTys
        ///variable, static field, or static (non-parameterized) property
        | SynExpr.PathGet(path) -> //DONE
            match PR.tryResolveLeadingPathGet env path with
            | None ->
                EM.Variable_field_or_property_not_found path.Pos path.Text
                abort()
            | Some(ilExpr, Some(rest)) -> 
                PR.resolveILExprInstancePathGet ilExpr rest
            | Some(ilExpr, None) ->
                ilExpr
        | SynExpr.ExprPathGet(x, path) -> //DONE
            let x = semantExpr x
            PR.resolveILExprInstancePathGet x path
        | SynExpr.PathSet(path, (assign, assignPos)) ->
            let assign = semantExpr assign

            let instance =
                match path.LeadingPartsPath with
                | Some(path) -> 
                    match PR.tryResolveLeadingPathGet env path with
                    | None ->
                        None
                    | Some(ilExpr, Some(rest)) -> 
                        Some(PR.resolveILExprInstancePathGet ilExpr rest)
                    | Some(ilExpr, None) ->
                        Some(ilExpr)
                | None -> None

            match instance with
            | Some(instance) ->
                let path = path.LastPartPath
                PR.resolveILExprInstancePathSet instance path assign assignPos
            | None ->
                match PR.tryResolveStaticVarFieldOrProperty env path with
                | None ->
                    EM.Variable_field_or_property_not_found path.Pos path.Text
                    ILExpr.Error(typeof<Void>)
                | Some(vfp) ->
                    match vfp with
                    | PR.VFP.Var(name,ty) ->
                        if not <| ty.IsAssignableFrom(assign.Type) then
                            EM.Variable_set_type_mismatch path.Pos path.Text ty.Name assign.Type.Name
                            ILExpr.Error(typeof<Void>)
                        else
                            ILExpr.VarSet(name, castIfNeeded ty assign)
                    | PR.VFP.FieldOrProperty(PR.FP.Field(fi)) ->
                        let path = path.LastPartPath
                        PR.validateFieldSet fi path assign.Type assignPos
                            (lazy(ILExpr.StaticFieldSet(fi, castIfNeeded fi.FieldType assign)))
                    | PR.VFP.FieldOrProperty(PR.FP.Property(pi)) ->
                        let path = path.LastPartPath
                        PR.validatePropertySet pi path assign.Type assignPos
                            (lazy(ILExpr.StaticPropertySet(pi, castIfNeeded pi.PropertyType assign)))
        | SynExpr.ExprPathSet(instance, path, (assign,assignPos)) -> //TODO
            let instance = semantExpr instance
            let assign = semantExpr assign
            match path.LeadingPartsPath with
            | Some(leadingPath) -> 
                let instance = PR.resolveILExprInstancePathGet instance leadingPath
                PR.resolveILExprInstancePathSet instance path.LastPartPath assign assignPos
            | None ->
                PR.resolveILExprInstancePathSet instance path assign assignPos
        | SynExpr.Let(name, (assign, assignPos), body) ->
            let assign = semantExpr assign
            if assign.Type = typeof<Void> then
                EM.Void_invalid_in_let_binding assignPos
        
            let body = semantExprWith (env.ConsVariable(name, assign.Type)) body
            ILExpr.Let(name, assign, body, body.Type)
        | SynExpr.Sequential((SynExpr.Break(_)|SynExpr.Continue(_)) as x, (_,pos)) ->
            EM.Unreachable_code_detected pos
            semantExpr x //error recovery
        | SynExpr.Sequential(x,(y,_)) ->
            let x, y = semantExpr x, semantExpr y
            ILExpr.Sequential(x,y,y.Type)
        | SynExpr.OpenNamespaceOrType(nsOrTy, x) ->
            match nsOrTy.GenericArgs with
            | [] when namespaceExists env.Assemblies nsOrTy.GenericName ->
                semantExprWith (env.ConsNamespace(nsOrTy.GenericName)) x
            | _ ->
                let tyTys = resolveTySigs env nsOrTy.GenericArgs
                match tryResolveType env.Namespaces env.Assemblies nsOrTy.GenericName tyTys with
                | Some(ty) ->
                    semantExprWith (env.ConsType(ty)) x
                | None ->
                    EM.Namespace_or_type_not_found nsOrTy.Pos nsOrTy.Name (sprintAssemblies env.Assemblies)
                    semantExpr x
        | SynExpr.OpenAssembly((name,pos), x) ->
            let asm = tryLoadAssembly name
            match asm with
            | None -> 
                EM.Could_not_resolve_assembly pos name
                semantExpr x
            | Some(asm) -> semantExprWith {env with Assemblies=asm::env.Assemblies} x
        | SynExpr.LogicalNot(x,pos) ->
            let x = semantExpr x
            if x.Type <> typeof<bool> then
                EM.Expected_type_but_got_type pos "System.Boolean" x.Type.Name
                ILExpr.Error(typeof<bool>)
            else
                ILExpr.LogicalNot(x)
        | SynExpr.Cast(x, tySig, pos) ->
            let x = semantExpr x
            let ty = resolveTySig env tySig
            
            if ty = typeof<System.Void> then
                EM.Casting_to_void_invalid pos
                ILExpr.Error(ty)
            elif x.Type = ty then
                EM.Casting_noop pos x.Type.Name
                x
            elif ty.IsAssignableFrom(x.Type) || x.Type.IsAssignableFrom(ty) then
                ILExpr.Cast(x,ty)
            //implicit or explicit coersion of numeric primitive to numeric primitive or char to numeric primitive (under-the-hood chars are ints)
            elif NumericPrimitive.sourceHasImplicitOrExplicitConvTo x.Type ty || (x.Type = typeof<char> && NumericPrimitive.isNumericPrimitive ty) then
                ILExpr.Coerce(env.Checked,x,ty)
            else
                let meth = seq {
                    //giver implicit conversion op from either type over explicit ops; next prefer conversion op defined on lhs type over rhs type
                    yield tryResolveOpImplicit ty x.Type ty
                    yield tryResolveOpImplicit x.Type x.Type ty
                    yield tryResolveOpExplicit ty x.Type ty
                    yield tryResolveOpExplicit x.Type x.Type ty } |> Seq.tryPick id

                match meth with
                | Some(meth) -> 
                    ILExpr.StaticCall(meth, [x])
                | None -> 
                    EM.Casting_from_type_to_type_always_invalid pos x.Type.Name ty.Name
                    ILExpr.Error(ty)
        | SynExpr.LogicBinop(op,(x,xpos),(y,ypos)) ->
            let x =
                match semantExpr x with
                | x when x.Type <> typeof<bool> -> 
                    EM.Expected_type_but_got_type xpos "System.Boolean" x.Type.Name
                    ILExpr.Error(typeof<bool>)
                | x -> x

            let y = 
                match semantExpr y with
                | y when y.Type <> typeof<bool> ->
                    EM.Expected_type_but_got_type ypos "System.Boolean" y.Type.Name
                    ILExpr.Error(typeof<bool>)
                | y -> y
        
            match op with
            | SynLogicBinop.And -> ILExpr.IfThenElse(x, y, ILExpr.Bool(false), typeof<bool>)
            | SynLogicBinop.Or -> ILExpr.IfThenElse(x, ILExpr.Bool(true), y, typeof<bool>)
        | SynExpr.IfThenElse((condition, conditionPos),thenBranch,elseBranch,pos) ->
            let condition = 
                let condition = semantExpr condition
                if condition.Type <> typeof<bool> then
                    EM.Expected_type_but_got_type conditionPos "System.Boolean" condition.Type.Name
                    ILExpr.Error(typeof<bool>)
                else
                    condition
            let thenBranch = semantExpr thenBranch
            match elseBranch with
            | Some(elseBranch) ->
                let elseBranch = semantExpr elseBranch
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
                let condition = semantExpr condition
                if condition.Type <> typeof<bool> then
                    EM.Expected_type_but_got_type conditionPos "System.Boolean" condition.Type.Name
                    ILExpr.Error(typeof<bool>)
                else
                    condition
            let body = semantExprWith {env with IsLoopBody=true} body
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
        | SynExpr.Checked(x) ->
            semantExprWith {env with Checked=true} x
        | SynExpr.Unchecked(x) ->
            semantExprWith {env with Checked=false} x
        | SynExpr.Throw(x, pos) ->
            let x = semantExpr x
            if typeof<Exception>.IsAssignableFrom(x.Type) then
                ILExpr.Throw(x)
            else
                EM.Throw_type_does_not_extend_Exception pos (x.Type.ToString())
                ILExpr.Error(typeof<Void>)

    match synTopLevel with
    | SynTopLevel.StmtList(xl) ->
        let rec loop env synStmts ilStmts =
            match synStmts with
            | [] -> ilStmts |> List.rev
            | synStmt::synStmts ->
                match synStmt with
                | SynStmt.Do x ->
                    let ilStmt = ILStmt.Do(semantExprWith env x)
                    loop env synStmts (ilStmt::ilStmts)                
                | SynStmt.Let(name, (assign,assignPos)) ->
                    let assign = semantExprWith env assign
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
        ILTopLevel.Expr(semantExprWith env x)