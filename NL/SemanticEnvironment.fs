namespace Swensen.NL
open System.Reflection
open System

type NVT =
    | Namespace of string
    | Variable of string * Type
    | Type of Type

///Environmental context passed to each recursive expression process during semantic analysis
type SemanticEnvironment = 
    {
        ///Indicates that the current expression is inside a loop body
        IsLoopBody: bool
        ///All "open" loaded assemblies
        Assemblies: Assembly list
        
        ///All "open" namespaces, variables, and types
        NVTs: NVT list
    }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
module SemanticEnvironment =
    ///The "empty" semantic environment
    let Empty = { IsLoopBody=false; Assemblies=[]; NVTs=[] }
    ///The "default" / initial semantic environment
    let Default =
        { Empty with 
            Assemblies=
                (["mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System.Core, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System.Numerics, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"] |> List.map Assembly.Load)
            NVTs=
                ["system"
                 "system.collections"
                 "system.collections.generic"
                 "system.numerics"] |> List.map NVT.Namespace }

type SemanticEnvironment with
    member this.Namespaces = 
        this.NVTs 
        |> List.choose (function NVT.Namespace(ns) -> Some(ns) | _ -> None)
    
    member this.Variables = 
        this.NVTs 
        |> List.choose (function NVT.Variable(name,ty) -> Some(name,ty) | _ -> None) |> Map.ofList
    
    member this.Types = 
        this.NVTs 
        |> List.choose (function NVT.Type(ty) -> Some(ty) | _ -> None) 

    member this.ConsNamespace(ns)       = { this with NVTs= NVT.Namespace(ns)::this.NVTs }
    member this.ConsVariable(name, ty)  = { this with NVTs= NVT.Variable(name,ty)::this.NVTs }
    member this.ConsType(ty)            = { this with NVTs= NVT.Type(ty)::this.NVTs }

    member this.ConsAssembly(assm)      = { this with Assemblies= assm::this.Assemblies }




