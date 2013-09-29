namespace Swensen.NL
open System.Reflection
open System.Reflection.Emit
open System

//todo: fix problem with new methods: http://stackoverflow.com/a/288928/236255

[<AbstractClass>]
///Construct a type manager from a given type (may not be null).
type TypeManager(ty:Type) =
    do if ty = null then raise (ArgumentNullException("ty"))
    //todo: incomplete, needs to be more rigorous. e.g. hides-by-sig-and-name always (c# semantics but not vb semantics considered)
    //does not consider any meta data other than just the ret and param and generic arg types
    let discardHideBySigMembers ls =
        ls
        |> Seq.distinctByResolve
            (fun (x:MethodInfo) ->
                let paramTys = x.GetParameters() |> Array.map (fun p -> p.ParameterType)
                let gargTys = if x.IsGenericMethod then x.GetGenericArguments() else [||]
                x.Name, x.ReturnType, gargTys, paramTys)
            (fun x y -> 
                if x.DeclaringType = y.DeclaringType then 0
                elif x.DeclaringType.IsAssignableFrom(y.DeclaringType) then -1
                else 1)
    ///search for members by name: first try case sensitive search and if no matches, then try case-insensitive search.
    let searchByName searchName (ls: #MemberInfo list) =
        let matchesExact = ls |> List.filter (fun m -> m.Name = searchName)
        let matches = 
            if matchesExact.Length = 0 then 
                ls |> List.filter (fun m -> String.Equals(m.Name, searchName, StringComparison.OrdinalIgnoreCase))
            else 
                matchesExact
        matches
    ///search for a method base (method or ctor) by params: params length matches argTys length and then we first try
    ///exact equals match of params types and argTys and if no matches then we try param tys assignable to arg tys.
    let searchByParams argTys (ls:#MethodBase list) = 
        let matchesByNumber = ls |> List.filter (fun m -> m.GetParameters().Length = Seq.length argTys)
        let matchesExact = 
            matchesByNumber 
            |> List.filter (fun m -> 
                Seq.zip (m.GetParameters()) argTys |> Seq.forall (fun (p,aty) -> p.ParameterType = aty))
        let matches =   
            if matchesExact.Length = 0 then
                matchesByNumber 
                |> List.filter (fun m -> 
                    Seq.zip (m.GetParameters()) argTys |> Seq.forall (fun (p,aty) -> p.ParameterType.IsAssignableFrom(aty)))
            else
                matchesExact
        matches
    abstract Fields: FieldInfo seq 
    abstract Properties : PropertyInfo seq 
    abstract Methods: MethodInfo seq   
    abstract Constructors: ConstructorInfo seq 
    member this.Type = ty
    member this.FindAllProperties(searchName:string, searchAttributes:MethodAttributes) =
        this.Properties 
        |> Seq.toList 
        |> searchByName searchName
        |> List.filter (fun p -> 
            p.CanRead && p.GetMethod.Attributes.HasFlag(searchAttributes) ||
            p.CanWrite && p.SetMethod.Attributes.HasFlag(searchAttributes))
    member this.TryFindProperty(searchName, searchAttributes) =
        this.FindAllProperties(searchName, searchAttributes) 
        |> List.tryHead
    member this.FindAllFields(searchName:string, searchAttributes:FieldAttributes) =
        this.Fields 
        |> Seq.toList 
        |> searchByName searchName
        |> List.filter(fun m -> m.Attributes.HasFlag(searchAttributes))
    member this.TryFindField(searchName:string, searchAttributes:FieldAttributes) =
        this.FindAllFields(searchName, searchAttributes)
        |> List.tryHead
    member this.FindAllConstructors(argTys: Type seq, searchAttributes:MethodAttributes) =
        this.Constructors 
        |> Seq.toList 
        |> List.filter(fun m -> m.Attributes.HasFlag(searchAttributes))
        |> searchByParams argTys
    member this.TryFindConstructor(argTys: Type seq, searchAttributes:MethodAttributes) =
        this.FindAllConstructors(argTys, searchAttributes)
        |> List.tryHead
    member this.FindAllMethods(searchName, genericTyArgs: Type seq, argTys: Type seq, retTy : Type option, searchAttributes:MethodAttributes) =
        this.Methods 
        |> Seq.toList 
        |> searchByName searchName
        |> List.filter(fun m -> m.Attributes.HasFlag(searchAttributes))
        |> (fun matches ->
                if genericTyArgs |> Seq.length > 0 then 
                    matches
                    |> List.filter (fun meth -> 
                        meth.IsGenericMethod && 
                        meth.GetGenericArguments().Length = Seq.length genericTyArgs) 
                    |> List.map (fun meth -> meth.MakeGenericMethod(genericTyArgs |> Seq.toArray))
                else
                    matches)
        |> searchByParams argTys
        |> (fun matches ->
                match retTy with
                | Some(retTy) -> 
                    let matches = 
                        matches |> List.filter (fun meth -> retTy = meth.ReturnType)
                    if matches.Length = 0 then
                        matches |> List.filter (fun meth -> retTy.IsAssignableFrom(meth.ReturnType))
                    else
                        matches
                | None -> matches)
        |> discardHideBySigMembers 
        |> Seq.toList
    member this.TryFindMethod(searchName, genericTyArgs: Type seq, argTys: Type seq, retTy : Type option, searchAttributes:MethodAttributes) =
        this.FindAllMethods(searchName,genericTyArgs,argTys,retTy,searchAttributes)
        |> List.tryHead
    override this.ToString() = this.Type.ToString()
    ///Equality based on underlying Type.
    override this.Equals(other:obj) = 
        match other with
        | :? TypeManager as other -> this.Type = other.Type
        | _ -> false
    override this.GetHashCode() = this.Type.GetHashCode()

//todo implement semantics similar to RunTimeTypeBuilder for returned members
//(e.g. filter out overriden methods, etc.).
///A mutable TypeManager used for managing TypeBuilders.
type TypeBuilderManager(ty:TypeBuilder) =
    inherit TypeManager(ty)
    let mutable fields = ResizeArray<FieldBuilder>()
    let mutable methods = ResizeArray<MethodBuilder>()
    let mutable properties = ResizeArray<PropertyBuilder>()
    let mutable constructors = ResizeArray<ConstructorBuilder>()
    member this.TypeBuilder = ty
    member this.AddField(field:FieldBuilder) = fields.Add(field)
    member this.AddMethod(meth:MethodBuilder) = methods.Add(meth)
    member this.AddProperty(property:PropertyBuilder) = properties.Add(property)
    member this.AddConstructor(ctor:ConstructorBuilder) = constructors.Add(ctor)
    override this.Fields = fields |> Seq.readonly |> Seq.cast<FieldInfo>
    override this.Methods = methods |> Seq.readonly |> Seq.cast<MethodInfo>
    override this.Properties = properties |> Seq.readonly |> Seq.cast<PropertyInfo>
    override this.Constructors = constructors |> Seq.readonly |> Seq.cast<ConstructorInfo>
//    interface IComparable with
//        member this.CompareTo(that) =
//            match that with
//            | :? TypeBuilderManager as that ->
//                this.TypeBuilder.AssemblyQualifiedName.CompareTo(that.TypeBuilder.AssemblyQualifiedName)
//            | _ -> 1

//todo implement "hide-by-sig" semantics on all members (currently on methods)
///An immutable TypeManager used for managing RuntimeTypes (i.e. statically defined types)
type RuntimeTypeManager(ty:Type) =
    inherit TypeManager(ty)
        
    override this.Fields = ty.GetFields() |> Seq.readonly
    override this.Methods = ty.GetMethods() |> Seq.readonly
    override this.Properties = ty.GetProperties() |> Seq.readonly
    override this.Constructors = ty.GetConstructors() |> Seq.readonly

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeManager =
    let from (ty:Type) =
        match ty with
        | :? TypeBuilder as ty -> TypeBuilderManager(ty) :> TypeManager
        | _ -> RuntimeTypeManager(ty) :> TypeManager