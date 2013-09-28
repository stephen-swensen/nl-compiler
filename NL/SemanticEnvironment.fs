namespace Swensen.NL
open System.Reflection
open System.Reflection.Emit
open System

type NVT =
    | Namespace of string
    | Variable of string * Type
    | Type of Type

///Environmental context passed to each recursive expression process during semantic analysis
type SemanticEnvironment = 
    {
        ///Indicates that the current expression is inside a finally body of the current exception handler
        IsFinallyBodyOfCurrentExceptionHandler: bool
        ///Indicates that the current expression is inside a catch body
        IsCatchBody: bool
        ///Indicates that the current expression is inside a checked body
        Checked: bool
        ///Indicates that the current expression is inside a loop body
        IsLoopBody: bool
        //All TypeBuilders in the current AssemblyBuilder context
        TypeBuilders: TypeBuilderManager list
        ///All "open" loaded assemblies
        Assemblies: Assembly list
        ///All "open" namespaces, variables, and types
        NVTs: NVT list
    }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
module SemanticEnvironment =
    ///The "empty" semantic environment
    let Empty = { TypeBuilders=[]; IsFinallyBodyOfCurrentExceptionHandler=false; IsCatchBody=false; Checked=false; IsLoopBody=false; Assemblies=[]; NVTs=[] }
    ///The "default" / initial semantic environment
    let Default =
        { Empty with 
            Assemblies=
                (["mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System.Core, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System.Numerics, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"] |> List.map Assembly.Load)
            NVTs= //assuming that none of these namespaces have any colliding types (i.e. order doesn't matter here as far as possible shadowing)!
                ["" //n.b. System.Type.GetType(".system.string",false,true) IS valid (the leading ".", that is)
                 "system"
                 "system.collections.generic"
                 "system.numerics"] |> List.map NVT.Namespace }

type SemanticEnvironment with
    member this.Namespaces = 
        this.NVTs 
        |> Seq.choose (function NVT.Namespace(ns) -> Some(ns) | _ -> None)
    
    member this.Variables = 
        this.NVTs 
        |> Seq.choose (function NVT.Variable(name,ty) -> Some(name,ty) | _ -> None) |> Map.ofSeq
    
    member this.Types =
        this.NVTs 
        |> Seq.choose (function NVT.Type(ty) -> Some(ty) | _ -> None) 

    member this.ConsNamespace(ns) = { this with NVTs= NVT.Namespace(ns)::this.NVTs }
    member this.ConsVariable(name, ty) = { this with NVTs= NVT.Variable(name,ty)::this.NVTs }
    member this.ConsType(ty) = { this with NVTs= NVT.Type(ty)::this.NVTs }
    
    member this.ConsAssembly(assm) = { this with Assemblies= assm::this.Assemblies }
    member this.ConsTypeBuilder(tb) = { this with TypeBuilders= tb::this.TypeBuilders }

    member this.GetTypeManager(ty:Type) =
        let result = 
            this.TypeBuilders
            |> List.tryFind (fun tb -> tb.Type = ty)
        match result with
        | Some(tm) -> tm :> TypeManager
        | None -> TypeManager.from ty
        