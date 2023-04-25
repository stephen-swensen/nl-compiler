module Swensen.NL.SemanticAnalysis

open System
open System.Reflection
open System.Reflection.Emit
open Swensen.NL.Ast
open Swensen.NL.Ail

module CM = CompilerMessages
exception CompilerInterruptException
let abort() = raise CompilerInterruptException

let sprintSeqForDisplay xs f =
    if Seq.isEmpty xs then "()"
    else (xs |> Seq.map (fun x -> sprintf "'%s'" (f x)) |> String.concat ", ")

let sprintTypes (tarr:Type seq) =
    sprintSeqForDisplay tarr (fun ty -> ty.Name)

let sprintTySigs (tarr:TySig seq) =
    sprintSeqForDisplay tarr (fun ty -> ty.Name)

let sprintAssemblies (tarr:Assembly seq) =
    sprintSeqForDisplay tarr (fun asm -> asm.FullName)

let staticBindingFlags = BindingFlags.Public ||| BindingFlags.Static

///Binding flags for our language
let instanceFieldAttributes = FieldAttributes.Public
let staticFieldAttributes = FieldAttributes.Public ||| FieldAttributes.Static
let instanceMethodAttributes = MethodAttributes.Public
let staticMethodAttributes = MethodAttributes.Public ||| MethodAttributes.Static

let coerceIfNeeded cked targetTy (sourceExp:ILExpr) =
    if sourceExp.Type <> targetTy then Coerce(cked, sourceExp,targetTy) else sourceExp

///Unchecked cast sourceExp to targetTy
let castIfNeeded targetTy (sourceExp:ILExpr) =
    if sourceExp.Type <> targetTy then Cast(sourceExp,targetTy) else sourceExp

///SHOULD MAKE A "CAST OR COERCE IF NEEDED" METHOD!! BUG 31
let castArgsIfNeeded (targetParameterInfo:ParameterInfo[]) sourceArgExps =
    List.zip (targetParameterInfo |> Seq.map (fun p -> p.ParameterType) |> Seq.toList)  sourceArgExps
    |> List.map (fun (targetTy, sourceExp) -> castIfNeeded targetTy sourceExp)

let tryResolveMethod (env:SemanticEnvironment) (ty:Type) (name:string) methodAttributes (genericTyArgs:Type[]) (argTys: Type[]) =
    env.GetTypeManager(ty).TryFindMethod(name, genericTyArgs, Some(argTys), None, methodAttributes)

let tryResolveGetter (env:SemanticEnvironment) (ty:Type) (name:string) methodAttributes =
    env.GetTypeManager(ty).TryFindGetter(name, methodAttributes)

let tryResolveOpImplicit, tryResolveOpExplicit =
    let tryResolveConversionOp name (onty:Type) fromty toty =
        onty.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
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

//todo: support search through TypeManagers
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

let tryResolveField (env:SemanticEnvironment) (ty:Type) fieldName bindingFlags =
    env.GetTypeManager(ty).TryFindField(fieldName, bindingFlags)

//only resolves simple non-parameterized properties
let tryResolveProperty (env:SemanticEnvironment) (ty:Type) (propertyName:string) (searchAttributes:MethodAttributes)=
    env.GetTypeManager(ty).TryFindProperty(propertyName,searchAttributes)

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
            CM.Could_not_resolve_type nr.Pos nr.Name
        abort()

let resolveTySig env (tySig:TySig) =
    let tyTys = resolveTySigs env tySig.GenericArgs
    match tryResolveType env.Namespaces env.Assemblies tySig.GenericName tyTys with
    | Some(ty) -> ty
    | None ->
        CM.Could_not_resolve_type tySig.Pos tySig.Name //todo: specific pos for ty name
        abort()

//let resolveType namespaces assemblies name tyTys (originalTySig:TySig) =
//    match tryResolveType namespaces assemblies name tyTys with
//    | Some(ty) -> ty
//    | None ->
//        CM.Could_not_resolve_type originalTySig.Pos originalTySig.Name //todo: specific pos for ty name
//        abort()

let resolveILExprStaticCall env ty methodName methodGenericTyArgs args argTys pos =
    match tryResolveMethod env ty methodName staticMethodAttributes methodGenericTyArgs argTys with
    | None ->
        CM.Invalid_static_method pos methodName ty.Name (sprintTypes argTys)
        abort()
    | Some(meth) ->
        ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args)

let resolveILExprInstanceCall env (instance:ILExpr) (methodName:Path) methodGenericTyArgs args argTys =
    match tryResolveMethod env instance.Type methodName.Text instanceMethodAttributes methodGenericTyArgs argTys with
    | None ->
        CM.Invalid_instance_method methodName.Pos methodName.Text instance.Type.Name (sprintTypes argTys)
        abort()
    | Some(meth) ->
        ILExpr.InstanceCall(instance, meth, castArgsIfNeeded (meth.GetParameters()) args)

module PathResolution =
    type FP =
        | Field of FieldInfo //N.B. CONSTANT FIELDS LIKE INT32.MAXVALUE CANNOT BE ACCESSED NORMALLY: CHECK FOR ISLITERAL FIELD ATTRIBUTE AND EMIT LITERAL VALUE (CAN USE GETVALUE REFLECTION) -- IF CAN'T DO THAT, CHECK FIELDINFO HANDLE AND IF THROWS WE KNOW WE NEED TO STOP
        | Property of PropertySearchResult

    let tryResolveFieldOrProperty (env:SemanticEnvironment) ty name fieldSearchAttributes propMethodSearchAttributes =
        match tryResolveField env ty name fieldSearchAttributes with
        | Some(fi) -> Some(FP.Field(fi))
        | None ->
            match tryResolveProperty env ty name propMethodSearchAttributes with
            | prop when prop.HasGetterOrSetter -> Some(FP.Property(prop))
            | _ -> None

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
                    match tryResolveFieldOrProperty env ty path.LastPartText staticFieldAttributes staticMethodAttributes with
                    | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                    | None -> None
                | None -> None
            | NVT.Type(ty) when path.IsSinglePart -> //field or property of an open type
                match tryResolveFieldOrProperty env ty path.LastPartText staticFieldAttributes staticMethodAttributes with
                | Some(fp) -> Some(VFP.FieldOrProperty(fp))
                | None -> None
            | _ -> None)

    let tryResolveILExprStaticFieldOrPropertyGet env ty name rest =
        match tryResolveField env ty name staticFieldAttributes with
        | Some(fi) -> Some(ILExpr.mkStaticFieldGet(fi), rest)
        | None ->
            match tryResolveGetter env ty name staticMethodAttributes with
            | Some getter -> Some(ILExpr.StaticPropertyGet(getter),rest)
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
                | Some(ty) -> tryResolveILExprStaticFieldOrPropertyGet env ty path.LastPartText rest
                | None -> None
            //field or property of an open type
            | NVT.Type(ty) when path.IsSinglePart->
                tryResolveILExprStaticFieldOrPropertyGet env ty path.LastPartText rest
            | _ -> None)

    let rec tryResolveILExprInstancePathGet env (ilExpr:ILExpr) (path:Path) =
        let path,rest = path.FirstPartPathWithRest
        let ilExpr =
            match tryResolveField env ilExpr.Type path.Text instanceFieldAttributes with
            | Some(fi) -> Some(ILExpr.InstanceFieldGet(ilExpr,fi))
            | None ->
                match tryResolveGetter env ilExpr.Type path.Text instanceMethodAttributes with
                | Some(getter) -> Some(ILExpr.InstancePropertyGet(ilExpr,getter))
                | None ->
                    None

        match ilExpr, rest with
        | Some(ilExpr), Some(rest) -> tryResolveILExprInstancePathGet env ilExpr rest
        | Some(ilExpr), None -> Some(ilExpr)
        | None, _ -> None

    //the reason we don't just implement this in terms of tryResolveILExprInstancePathGet is because we
    //want the precise point of ailure in the rest chain (of course, we could implement tryResolveILExprInstancePathGet
    //with more info returned...)
    let rec resolveILExprInstancePathGet env (ilExpr:ILExpr) (path:Path) =
        let path,rest = path.FirstPartPathWithRest
        let ilExpr =
            match tryResolveField env ilExpr.Type path.Text instanceFieldAttributes with
            | Some(fi) -> ILExpr.InstanceFieldGet(ilExpr,fi)
            | None ->
                match tryResolveGetter env ilExpr.Type path.Text instanceMethodAttributes with
                | Some(getter) -> ILExpr.InstancePropertyGet(ilExpr,getter)
                | None ->
                    CM.Instance_field_or_property_not_found path.Pos path.Text ilExpr.Type.Name
                    abort()

        match rest with
        | Some(rest) -> resolveILExprInstancePathGet env ilExpr rest
        | None -> ilExpr

    let validateFieldSet (fi:FieldInfo) (path:Path) (assignTy:Type) (assignPos) (ifValid:Lazy<_>) =
        if not <| fi.FieldType.IsAssignableFrom(assignTy) then //allow implicit cast?
            CM.Field_set_type_mismatch (PositionRange(path.Pos,assignPos)) path.Text fi.FieldType.Name assignTy.Name
            ILExpr.Error(typeof<Void>)
        else
            ifValid.Value

    let validatePropertySet instance (pi:PropertySearchResult) (path:Path) (assign:ILExpr) (assignPos) =
        match pi with
        | { Setter=Some(setter) } ->
            if not <| setter.Type.IsAssignableFrom(assign.Type) then //allow implicit cast?
                CM.Property_set_type_mismatch (PositionRange(path.Pos, assignPos)) path.Text setter.Type.Name assign.Type.Name
                ILExpr.Error(typeof<Void>)
            else
                ILExpr.PropertySet(instance, setter, castIfNeeded setter.Type assign)
        | _ ->
            CM.Property_has_no_setter path.Pos path.Text
            ILExpr.Error(typeof<Void>)

    let resolveILExprInstancePathSet env (instance:ILExpr) (path:Path) (assign:ILExpr) (assignPos) =
        match tryResolveFieldOrProperty env instance.Type path.Text instanceFieldAttributes instanceMethodAttributes with
        | Some(FP.Field(fi)) ->
            validateFieldSet fi path assign.Type assignPos
                (lazy(ILExpr.InstanceFieldSet(instance, fi, castIfNeeded fi.FieldType assign)))
        | Some(FP.Property(pi)) ->
            validatePropertySet (Some(instance)) pi path assign assignPos
        | None ->
            CM.Instance_field_or_property_not_found path.Pos path.Text instance.Type.Name
            ILExpr.Error(typeof<Void>)

    ///resolves the full path call not allowing any leading chaining
    let resolveFullPathCall env (path:Path) genericTyArgs args argTys pos =
        let attempt =
            env.NVTs
            |> Seq.tryPick (function
                | NVT.Variable(name,ty) when name = path.LeadingPartsText -> //instance method call on variable
                    let instance = ILExpr.VarGet(path.LeadingPartsText,ty)
                    let methodName = path.LastPartPath
                    Some(resolveILExprInstanceCall env instance methodName genericTyArgs args argTys)
                | NVT.Namespace(ns) ->
                    match tryResolveType [ns] env.Assemblies path.LeadingPartsText [] with
                    | Some(ty) -> //static method call (possibly generic) on non-generic type (need to handle generic type in another parse case, i think)
                        Some(resolveILExprStaticCall env ty path.LastPartText genericTyArgs args argTys pos)
                    | None -> //constructors (generic or non-generic)
                        match tryResolveType [ns] env.Assemblies path.Text genericTyArgs with
                        | None ->
                            None
                        | Some(ty) ->
                            if ty.IsValueType && args.Length = 0 then
                                if ty = typeof<System.Void> then
                                    CM.Void_cannot_be_instantiated pos
                                    Some(ILExpr.Error(ty))
                                else
                                    Some(ILExpr.Default(ty))
                            else
                                match ty.GetConstructor(argTys) with
                                | null ->
                                    CM.Could_not_resolve_constructor pos ty.Name (args |> List.map(fun arg -> arg.Type) |> sprintTypes)
                                    Some(ILExpr.Error(ty))
                                | ctor ->
                                    Some(ILExpr.Ctor(ctor, castArgsIfNeeded (ctor.GetParameters()) args, ty))
                | NVT.Type(ty) when path.IsSinglePart -> //static methods of an open type
                    match tryResolveMethod env ty path.LastPartText staticMethodAttributes genericTyArgs argTys with
                    | Some(meth) ->
                        Some(ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) args))
                    | None ->
                        None
                | _ ->
                    None)

        match attempt with
        | None ->
            CM.Could_not_resolve_possible_method_call_or_contructor_type pos path.LeadingPartsText path.Text
            abort()
        | Some(ilExpr) ->
            ilExpr

module PR = PathResolution

//we need to do numeric parsing here instead of in the lexer so that the grammer handles uminus vs. minus correctly
module NumericParsing =
    let parseSByte input pos =
        match System.SByte.TryParse(input:string) with
        | true, value -> ILExpr.SByte(value)
        | _ ->
            CM.SByte_literal_out_of_range pos input
            ILExpr.SByte(0y) //error recovery

    let parseByte input pos =
        match System.Byte.TryParse(input:string) with
        | true, value -> ILExpr.Byte(value)
        | _ ->
            CM.Byte_literal_out_of_range pos input
            ILExpr.Byte(0uy) //error recovery

    let parseInt16 input pos =
        match System.Int16.TryParse(input:string) with
        | true, value -> ILExpr.Int16(value)
        | _ ->
            CM.Int16_literal_out_of_range pos input
            ILExpr.Int16(0s) //error recovery

    let parseInt32 input pos =
        match System.Int32.TryParse(input:string) with
        | true, value -> ILExpr.Int32(value)
        | _ ->
            CM.Int32_literal_out_of_range pos input
            ILExpr.Int32(0) //error recovery

    let parseInt64 input pos =
        match System.Int64.TryParse(input:string) with
        | true, value -> ILExpr.Int64(value)
        | _ ->
            CM.Int64_literal_out_of_range pos input
            ILExpr.Int64(0L) //error recovery

    let parseUInt16 input pos =
        match System.UInt16.TryParse(input:string) with
        | true, value -> ILExpr.UInt16(value)
        | _ ->
            CM.UInt16_literal_out_of_range pos input
            ILExpr.UInt16(0us) //error recovery

    let parseUInt32 input pos =
        match System.UInt32.TryParse(input:string) with
        | true, value -> ILExpr.UInt32(value)
        | _ ->
            CM.UInt32_literal_out_of_range pos input
            ILExpr.UInt32(0u) //error recovery

    let parseUInt64 input pos =
        match System.UInt64.TryParse(input:string) with
        | true, value -> ILExpr.UInt64(value)
        | _ ->
            CM.UInt64_literal_out_of_range pos input
            ILExpr.UInt64(0UL) //error recovery

    let parseSingle input pos =
        match System.Single.TryParse(input:string) with
        | true, value when value <> Single.PositiveInfinity && value <> Single.NegativeInfinity ->
            ILExpr.Single(value)
        | _ ->
            CM.Single_literal_out_of_range pos input
            ILExpr.Single(0.0f) //error recovery

    let parseDouble input pos =
        match System.Double.TryParse(input:string) with
        | true, value when value <> Double.PositiveInfinity && value <> Double.NegativeInfinity ->
            ILExpr.Double(value)
        | _ ->
            CM.Double_literal_out_of_range pos input
            ILExpr.Double(0.0) //error recovery

let semantExprWith env synExpr =
    let rec semantExprWith env synExpr =
        let semantExpr = semantExprWith env

        match synExpr with
        | SynExpr.Byte(x,pos) -> NumericParsing.parseByte x pos
        | SynExpr.SByte(x,pos) -> NumericParsing.parseSByte x pos
        | SynExpr.Int16(x,pos) -> NumericParsing.parseInt16 x pos
        | SynExpr.Int32(x,pos) -> NumericParsing.parseInt32 x pos
        | SynExpr.Int64(x,pos) -> NumericParsing.parseInt64 x pos
        | SynExpr.UInt16(x,pos) -> NumericParsing.parseUInt16 x pos
        | SynExpr.UInt32(x,pos) -> NumericParsing.parseUInt32 x pos
        | SynExpr.UInt64(x,pos) -> NumericParsing.parseUInt64 x pos
        | SynExpr.Single(x,pos) -> NumericParsing.parseSingle x pos
        | SynExpr.Double(x,pos) -> NumericParsing.parseDouble x pos
        | SynExpr.UMinus(SynExpr.Byte(x,xpos),upos) -> NumericParsing.parseByte ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.SByte(x,xpos),upos) -> NumericParsing.parseSByte ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Int16(x,xpos),upos) -> NumericParsing.parseInt16 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Int32(x,xpos),upos) -> NumericParsing.parseInt32 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Int64(x,xpos),upos) -> NumericParsing.parseInt64 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.UInt16(x,xpos),upos) -> NumericParsing.parseUInt16 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.UInt32(x,xpos),upos) -> NumericParsing.parseUInt32 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.UInt64(x,xpos),upos) -> NumericParsing.parseUInt64 ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Single(x,xpos),upos) -> NumericParsing.parseSingle ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.UMinus(SynExpr.Double(x,xpos),upos) -> NumericParsing.parseDouble ("-"+x) (PositionRange(upos,xpos))
        | SynExpr.String(x) -> ILExpr.String(x)
        | SynExpr.Char(x) -> ILExpr.Char(x)
        | SynExpr.Bool(x) -> ILExpr.Bool(x)
        | SynExpr.Null(tySig) ->
            let ty = resolveTySig env tySig
            if ty.IsValueType then
                CM.Null_is_invalid_for_value_types tySig.Pos ty.Name
                ILExpr.Null(ty) //error recovery: use wants to use a ValueType, but incorrectly wanted to use null for it
            else
                ILExpr.Null(ty)
        | SynExpr.Typeof(tySig)   ->
            match tryResolveTySig env tySig with
            | None ->
                CM.Could_not_resolve_type tySig.Pos tySig.Name
                ILExpr.Typeof(typeof<obj>) //error recovery: this is a runtime value that won't hurt us error
            | Some(ty) ->
                ILExpr.Typeof(ty)
        | SynExpr.Default(tySig)   ->
            let ty = resolveTySig env tySig
            if ty = typeof<System.Void> then
                CM.Void_cannot_be_instantiated tySig.Pos

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
                    CM.No_overload_found_for_unary_operator pos "-" x.Type.Name
                    ILExpr.Error(x.Type)
                | meth ->
                    ILExpr.StaticCall(meth, [x])
        | SynExpr.Pow(x, y, pos) ->
            let x,y = semantExpr x, semantExpr y
            //TODO: hmm, revisit this, i'm not so sure we want to pass in static types instead of true types of x and y, we know this should resolve
            match tryResolveMethod env typeof<System.Math> "Pow" staticMethodAttributes [||] [|typeof<float>;typeof<float>|] with
            | None ->
                CM.Internal_error pos "Failed to resolve 'System.Math.Pow(float,float)' for synthetic operator '**'"
                ILExpr.Error(typeof<float>)
            | Some(meth) ->
                if NumericPrimitive.sourceIsEqualOrHasImplicitConvToTarget x.Type typeof<Double>
                    && NumericPrimitive.sourceIsEqualOrHasImplicitConvToTarget y.Type typeof<Double> then
                    ILExpr.StaticCall(meth, [x;y] |> List.map (coerceIfNeeded env.Checked typeof<Double>))
                else
                    CM.No_overload_found_for_binary_operator pos "**" x.Type.Name y.Type.Name
                    ILExpr.Error(typeof<float>)
        | SynExpr.NumericBinop(op,x,y,pos) ->
            let x, y = semantExpr x, semantExpr y
            match NumericPrimitive.tryGetEqualOrImplicitConvTarget x.Type y.Type with
            | Some(targetTy) -> //primitive
                ILExpr.mkNumericBinop(env.Checked, op, coerceIfNeeded env.Checked targetTy x, coerceIfNeeded env.Checked targetTy y, targetTy)
            | None when op = SynNumericBinop.Plus && (x.Type = typeof<string> || y.Type = typeof<string>) -> //string
                let meth = tryResolveMethod env typeof<System.String> "Concat" staticMethodAttributes [||] [|x.Type; y.Type|]
                match meth with
                | None ->
                    //there should always be a String.Concat(obj,obj) overload
                    CM.Internal_error pos (sprintf "Could not resolve 'String.Concat' synthetic '+' overload for argument types %s" (sprintTypes [x.Type; y.Type]))
                    ILExpr.Error(typeof<string>) //error recovery
                | Some(meth) ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y])
            | None -> //static "op_*" overloads
                let meth = seq {
                    yield tryResolveMethod env x.Type op.Name staticMethodAttributes [||] [|x.Type; y.Type|]
                    yield tryResolveMethod env y.Type op.Name staticMethodAttributes [||] [|x.Type; y.Type|] } |> Seq.tryPick id

                match meth with
                | None ->
                    CM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
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
                    yield tryResolveMethod env x.Type op.Name staticMethodAttributes [||] [|x.Type; y.Type|]
                    yield tryResolveMethod env y.Type op.Name staticMethodAttributes [||] [|x.Type; y.Type|] } |> Seq.tryPick id

                match meth, op with
                | Some(meth), _ ->
                    ILExpr.StaticCall(meth, castArgsIfNeeded (meth.GetParameters()) [x;y])
                //reference equals or non-numeric primitive comparison between bools or chars
                | None, (SynComparisonBinop.Eq | SynComparisonBinop.Neq) when (x.Type.IsAssignableFrom(y.Type) || y.Type.IsAssignableFrom(x.Type)) && (not (x.Type.IsValueType <> y.Type.IsValueType)) ->
                    ILExpr.mkComparisonBinop(op, x, y)
                | None, _ ->
                    CM.No_overload_found_for_binary_operator pos op.Symbol x.Type.Name y.Type.Name
                    ILExpr.Error(typeof<bool>)
        | SynExpr.GenericTypeStaticCall(tyName, tyGenericTyArgs, methodName, methodGenericTyArgs, args, pos) -> //todo: need more position info for different tokens
            let tyGenericTyArgs = resolveTySigs env tyGenericTyArgs
            let methodGenericTyArgs = resolveTySigs env methodGenericTyArgs

            match tryResolveType env.Namespaces env.Assemblies tyName tyGenericTyArgs with
            | None ->
                CM.Could_not_resolve_type pos tyName //TODO: IMPROVE IDENT POS INFO
                abort()
            | Some(ty) ->
                let args = args |> List.map (semantExpr)
                let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
                resolveILExprStaticCall env ty methodName methodGenericTyArgs args argTys pos
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
                        PR.tryResolveILExprInstancePathGet env ilExpr rest
                    | Some(ilExpr, None) ->
                        Some(ilExpr)
                | None -> None

            match instance with
            | Some(instance) ->
                let methodName = path.LastPartPath
                resolveILExprInstanceCall env instance methodName methodGenericTyArgs args argTys
            | None ->
                PR.resolveFullPathCall env path methodGenericTyArgs args argTys pos
        | SynExpr.ExprPathCall(instance, path, methodGenericTyArgs, args, pos) ->
            let instance, methodName =
                match path.LeadingPartsPath with
                | Some(leadingPath) ->
                    semantExpr <| SynExpr.ExprPathGet(instance, leadingPath), path.LastPartPath
                | None ->
                    semantExpr instance, path

            let args = args |> List.map (semantExpr)
            let argTys = args |> Seq.map(fun arg -> arg.Type) |> Seq.toArray
            let methodGenericTyArgs = resolveTySigs env methodGenericTyArgs

            resolveILExprInstanceCall env instance methodName methodGenericTyArgs args argTys
        ///variable, static field, or static (non-parameterized) property
        | SynExpr.PathGet(path) ->
            match PR.tryResolveLeadingPathGet env path with
            | None ->
                CM.Variable_field_or_property_not_found path.Pos path.Text
                abort()
            | Some(ilExpr, Some(rest)) ->
                PR.resolveILExprInstancePathGet env ilExpr rest
            | Some(ilExpr, None) ->
                ilExpr
        | SynExpr.ExprPathGet(x, path) ->
            let x = semantExpr x
            PR.resolveILExprInstancePathGet env x path
        | SynExpr.PathSet(path, (assign, assignPos)) ->
            let assign = semantExpr assign

            let instance =
                match path.LeadingPartsPath with
                | Some(path) ->
                    match PR.tryResolveLeadingPathGet env path with
                    | None ->
                        None
                    | Some(ilExpr, Some(rest)) ->
                        Some(PR.resolveILExprInstancePathGet env ilExpr rest)
                    | Some(ilExpr, None) ->
                        Some(ilExpr)
                | None -> None

            match instance with
            | Some(instance) ->
                let path = path.LastPartPath
                PR.resolveILExprInstancePathSet env instance path assign assignPos
            | None ->
                match PR.tryResolveStaticVarFieldOrProperty env path with
                | None ->
                    CM.Variable_field_or_property_not_found path.Pos path.Text
                    ILExpr.Error(typeof<Void>)
                | Some(vfp) ->
                    match vfp with
                    | PR.VFP.Var(name,ty) ->
                        if not <| ty.IsAssignableFrom(assign.Type) then
                            CM.Variable_set_type_mismatch path.Pos path.Text ty.Name assign.Type.Name
                            ILExpr.Error(typeof<Void>)
                        else
                            ILExpr.VarSet(name, castIfNeeded ty assign)
                    | PR.VFP.FieldOrProperty(PR.FP.Field(fi)) ->
                        let path = path.LastPartPath
                        PR.validateFieldSet fi path assign.Type assignPos
                            (lazy(ILExpr.StaticFieldSet(fi, castIfNeeded fi.FieldType assign)))
                    | PR.VFP.FieldOrProperty(PR.FP.Property(pi)) ->
                        let path = path.LastPartPath
                        PR.validatePropertySet None pi path assign assignPos
        | SynExpr.ExprPathSet(instance, path, (assign,assignPos)) ->
            let instance = semantExpr instance
            let assign = semantExpr assign
            match path.LeadingPartsPath with
            | Some(leadingPath) ->
                let instance = PR.resolveILExprInstancePathGet env instance leadingPath
                PR.resolveILExprInstancePathSet env instance path.LastPartPath assign assignPos
            | None ->
                PR.resolveILExprInstancePathSet env instance path assign assignPos
        | SynExpr.Let(name, (assign, assignPos), body) ->
            let assign = semantExpr assign
            if isVoidOrEscapeTy assign.Type then
                CM.Void_invalid_in_let_binding assignPos

            let body = semantExprWith (env.ConsVariable(name, assign.Type)) body
            ILExpr.Let(name, assign, body, body.Type)
        | SynExpr.Sequential(x,(y,pos)) ->
            let x = semantExpr x

            match x.Type with
            | EscapeTy -> CM.Unreachable_code_detected pos
            | _ -> ()

            let y = semantExpr y
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
                    CM.Namespace_or_type_not_found nsOrTy.Pos nsOrTy.Name (sprintAssemblies env.Assemblies)
                    semantExpr x
        | SynExpr.OpenAssembly((name,pos), x) ->
            let asm = tryLoadAssembly name
            match asm with
            | None ->
                CM.Could_not_resolve_assembly pos name
                semantExpr x
            | Some(asm) -> semantExprWith {env with Assemblies=asm::env.Assemblies} x
        | SynExpr.LogicalNot(x,pos) ->
            let x = semantExpr x
            match x.Type with
            | BooleanTy -> ILExpr.LogicalNot(x)
            | _ ->
                CM.Expected_type_but_got_type pos typeof<bool>.Name x.Type.Name
                ILExpr.Error(typeof<bool>)
        | SynExpr.Cast(x, tySig, pos) ->
            let x = semantExpr x
            let ty = resolveTySig env tySig

            if ty = typeof<System.Void> then
                CM.Casting_to_void_invalid pos
                ILExpr.Error(ty)
            elif x.Type = ty then
                CM.Casting_noop pos x.Type.Name
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
                    CM.Casting_from_type_to_type_always_invalid pos x.Type.Name ty.Name
                    ILExpr.Error(ty)
        | SynExpr.LogicBinop(op,(x,xpos),(y,ypos)) ->
            let x =
                let x = semantExpr x
                match x.Type with
                | BooleanTy -> x
                | _ ->
                    CM.Expected_type_but_got_type xpos typeof<bool>.Name x.Type.Name
                    ILExpr.Error(typeof<bool>)

            let y =
                let y = semantExpr y
                match y.Type with
                | BooleanTy -> y
                | _ ->
                    CM.Expected_type_but_got_type ypos typeof<bool>.Name y.Type.Name
                    ILExpr.Error(typeof<bool>)

            match op with
            | SynLogicBinop.And -> ILExpr.IfThenElse(x, y, ILExpr.Bool(false), typeof<bool>)
            | SynLogicBinop.Or -> ILExpr.IfThenElse(x, ILExpr.Bool(true), y, typeof<bool>)
        | SynExpr.IfThenElse((condition, conditionPos),thenBranch,elseBranch,pos) ->
            let condition =
                let condition = semantExpr condition
                match condition.Type with
                | BooleanTy -> condition
                | _ ->
                    CM.Expected_type_but_got_type conditionPos typeof<bool>.Name condition.Type.Name
                    ILExpr.Error(typeof<bool>)

            let thenBranch = semantExpr thenBranch
            match elseBranch with
            | Some(elseBranch) ->
                let elseBranch = semantExpr elseBranch
                if thenBranch.Type <> elseBranch.Type then
                    match thenBranch.Type, elseBranch.Type with
                    | EscapeTy, retTy | retTy, EscapeTy ->
                        ILExpr.IfThenElse(condition,thenBranch,elseBranch,retTy)
                    | _ ->
                        CM.IfThenElse_branch_type_mismatch pos thenBranch.Type.Name elseBranch.Type.Name
                        ILExpr.IfThenElse(condition,thenBranch, ILExpr.Error(thenBranch.Type), thenBranch.Type)
                else
                    ILExpr.IfThenElse(condition,thenBranch,elseBranch,thenBranch.Type)
            | None ->
                match thenBranch.Type with
                | VoidOrEscapeTy ->
                    ILExpr.IfThen(condition, thenBranch)
                | _ ->
                    ILExpr.IfThenElse(condition, thenBranch, ILExpr.Default(thenBranch.Type), thenBranch.Type)
        | SynExpr.Nop ->
            ILExpr.Nop
        | SynExpr.WhileLoop((condition, conditionPos), body) ->
            let condition =
                let condition = semantExpr condition
                match condition.Type with
                | BooleanTy -> condition
                | _ ->
                    CM.Expected_type_but_got_type conditionPos typeof<bool>.Name condition.Type.Name
                    ILExpr.Error(typeof<bool>)
            let body = semantExprWith {env with IsLoopBody=true} body
            ILExpr.WhileLoop(condition, body)
        | SynExpr.Break(pos) ->
            if env.IsLoopBody then
                ILExpr.Break
            else
                CM.Break_outside_of_loop pos
                ILExpr.Error(typeof<Escape>)
        | SynExpr.Continue(pos) ->
            if env.IsLoopBody then
                ILExpr.Continue
            else
                CM.Continue_outside_of_loop pos
                ILExpr.Error(typeof<Escape>)
        | SynExpr.Checked(x) ->
            semantExprWith {env with Checked=true} x
        | SynExpr.Unchecked(x) ->
            semantExprWith {env with Checked=false} x
        | SynExpr.Throw(x, pos) ->
            let x = semantExpr x
            if typeof<Exception>.IsAssignableFrom(x.Type) then
                ILExpr.Throw(x)
            else
                CM.Throw_type_does_not_extend_Exception pos x.Type.Name
                ILExpr.Error(typeof<Escape>)
        | SynExpr.TryCatchFinally(tx, [], None, pos) ->
            let tx = semantExpr tx
            CM.Try_without_catch_or_finally pos
            ILExpr.Error(tx.Type)
        | SynExpr.TryCatchFinally(tx, catchList, fx, txPos) ->
            let env = { env with IsFinallyBodyOfCurrentExceptionHandler=false }
            let semantExpr = semantExprWith env

            let tx = semantExpr tx

            let firstNonEscapeTy =
                match tx.Type with
                | EscapeTy -> None
                | ty -> Some(ty)

            let catchList, firstNonEscapeTy =
                let rec loop xl acc firstNonEscapeTy =
                    match xl with
                    | [] -> acc, firstNonEscapeTy
                    | (filterTySig, name, catch, catchPos)::tl ->
                        let filterTy =
                            filterTySig
                            |> Option.map (resolveTySig env)
                            |> Option.getOrDefault (typeof<System.Exception>)

                        let env =
                            let env = {env with IsCatchBody=true}
                            match name with
                            | Some(name) -> env.ConsVariable(name, filterTy)
                            | None -> env

                        let catch = semantExprWith env catch
                        //insure consistent branch types w/ error correction in case of mismatch
                        let catch, firstNonEscapeTy =
                            match firstNonEscapeTy with
                            | Some(prevTy)  when prevTy <> catch.Type && catch.Type <> typeof<Escape> ->
                                CM.Inconsistent_try_catch_branch_types catchPos catch.Type.Name prevTy.Name
                                ILExpr.Error(prevTy), firstNonEscapeTy
                            | None when catch.Type <> typeof<Escape> -> catch, Some(catch.Type)
                            | _ -> catch, firstNonEscapeTy

                        match acc with
                        | (prevFilterTy:Type,_,_)::_ when prevFilterTy.IsAssignableFrom(filterTy) ->
                            CM.Unreachable_code_detected catchPos
                        | _ -> ()
                        loop tl ((filterTy, name, catch)::acc) firstNonEscapeTy

                let catchList, firstNonEscapeTy = loop catchList [] firstNonEscapeTy
                List.rev catchList, firstNonEscapeTy

            let fx = fx |> Option.map (semantExprWith {env with IsFinallyBodyOfCurrentExceptionHandler=true})

            let xty =
                match firstNonEscapeTy with
                | Some(ty) -> ty
                | _ -> typeof<Escape>

            ILExpr.TryCatchFinally(tx, catchList, fx, xty)
        | SynExpr.Rethrow pos ->
            match env with
            | {IsCatchBody=true; IsFinallyBodyOfCurrentExceptionHandler=false} ->
                ILExpr.Rethrow
            | {IsCatchBody=true; IsFinallyBodyOfCurrentExceptionHandler=true} ->
                CM.Rethrow_of_outer_catch_not_valid_inside_nested_finally_body pos
                ILExpr.Error(typeof<Escape>)
            | {IsCatchBody=false} ->
                CM.Rethrow_not_valid_outside_of_catch_body pos
                ILExpr.Error(typeof<Escape>)
    try
        semantExprWith env synExpr
    with CompilerInterruptException ->
        ILExpr.Error(typeof<obj>)

let semantStmtsWith env stmts (mbuilder:ModuleBuilder) nextTopLevelTypeName nextItName =
    let topLevelFieldAttrs = FieldAttributes.Public ||| FieldAttributes.Static
    let xl = stmts
    let rec loop env synStmts ilStmts =
        match synStmts with
        | [] -> ilStmts |> List.rev
        | synStmt::synStmts ->
            match synStmt with
            | SynStmt.Do x when env.IsAnalysisOnly ->
                let x = semantExprWith env x
                let env = env.ConsVariable(nextItName(), x.Type)
                loop env synStmts ilStmts
            | SynStmt.Do x ->
                let x = semantExprWith env x
                let tyBuilder = mbuilder.DefineType(nextTopLevelTypeName(), TypeAttributes.Public)
                let tbm = TypeBuilderManager(tyBuilder)
                let tyInitExpr =
                    if not <| isVoidOrEscapeTy x.Type then
                        let fi = tyBuilder.DefineField(nextItName(), x.Type, topLevelFieldAttrs)
                        tbm.AddField(fi)
                        ILExpr.StaticFieldSet(fi,x)
                    else
                        x
                let ilStmt = ILStmt.TypeDef(tyBuilder, [tyInitExpr], [])
                let env = env.ConsType(tyBuilder).ConsTypeBuilderManager(tbm)
                loop env synStmts (ilStmt::ilStmts)
            | SynStmt.Let(name, (assign,assignPos)) when env.IsAnalysisOnly ->
                let assign = semantExprWith env assign
                if isVoidOrEscapeTy assign.Type then
                    CM.Void_invalid_in_let_binding assignPos
                let env = env.ConsVariable(name, assign.Type)
                loop env synStmts ilStmts
            | SynStmt.Let(name, (assign,assignPos)) ->
                let assign = semantExprWith env assign
                if isVoidOrEscapeTy assign.Type then
                    CM.Void_invalid_in_let_binding assignPos

                let tyBuilder = mbuilder.DefineType(nextTopLevelTypeName(), TypeAttributes.Public)
                let tbm = TypeBuilderManager(tyBuilder)
                let fi = tyBuilder.DefineField(name, assign.Type, topLevelFieldAttrs)
                tbm.AddField(fi)
                let tyInitExpr = ILExpr.StaticFieldSet(fi,assign)
                let ilStmt = ILStmt.TypeDef(tyBuilder, [tyInitExpr], [])
                let env = env.ConsType(tyBuilder).ConsTypeBuilderManager(tbm)
                loop env synStmts (ilStmt::ilStmts)
            | SynStmt.OpenAssembly(name, pos) ->
                let asm = tryLoadAssembly name
                match asm with
                | None ->
                    CM.Could_not_resolve_assembly pos name
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
                        CM.Namespace_or_type_not_found nsOrTy.Pos nsOrTy.Name (sprintAssemblies env.Assemblies)
                        loop env synStmts ilStmts //error recovery
            | SynStmt.TypeDef(name, istmts) when env.IsAnalysisOnly ->
                loop env synStmts ilStmts
            | SynStmt.TypeDef(name, istmts) ->
                let tyBuilder = mbuilder.DefineType(name, TypeAttributes.Public)
                let tbm = TypeBuilderManager(tyBuilder)
                let env = env.ConsType(tyBuilder).ConsTypeBuilderManager(tbm)

                //declare the members
                let decs =
                    istmts
                    |> List.map (function
                        | SynTypeDefStmt.MemberDef(name, tySig, assign)->
                            let ty = resolveTySig env tySig
                            if isVoidOrEscapeTy ty then
                                CM.Void_invalid_in_let_binding tySig.Pos //TODO: make field specific message
                                abort()
                            let fi = tyBuilder.DefineField(name, ty, FieldAttributes.Public)
                            tbm.AddField(fi)
                            fi, assign) //cur only support field types, can expand to other members with DU

                //todo: may need an formal representation of "this" for optimizations
                //semant the istms and collect the ty inits
                let objinitExprs =
                    decs
                    |> List.map (function
                        | (fi:FieldBuilder,(assign,assignPos)) ->
                            let env = env.ConsVariable("this", tbm.Type)
                            let assign = semantExprWith env assign
                            if assign.Type <> fi.FieldType then
                                CM.Member_def_type_mismatch assignPos name fi.FieldType.Name assign.Type.Name
                            let objinitExpr = ILExpr.InstanceFieldSet(ILExpr.VarGet("this", tbm.Type), fi,assign)
                            objinitExpr
                        )

                let ilStmt = ILStmt.TypeDef(tyBuilder, [], objinitExprs)
                loop env synStmts (ilStmt::ilStmts)

    try
        loop env xl []
    with CompilerInterruptException ->
        [ILStmt.Error]