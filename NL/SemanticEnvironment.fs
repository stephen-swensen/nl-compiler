namespace Swensen.NL
open System.Reflection
open System

///Environmental context passed to each recursive expression process during semantic analysis
type SemanticEnvironment = 
    {
        ///Indicates that the current expression is inside a loop body
        IsLoopBody: bool
        ///All the visible referenced assemblies
        Assemblies: Assembly list
        ///All the open namespaces
        Namespaces: string list
        ///All the visible variables
        Variables: Map<string,Type>
    }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
module SemanticEnvironment =
    ///The "empty" semantic environment
    let Empty = { IsLoopBody=false; Assemblies=[]; Namespaces=[]; Variables= Map.empty }
    ///The "default" / initial semantic environment
    let Default =
        { Empty with 
            Assemblies=
                (["mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System.Core, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
                  "System.Numerics, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"] |> List.map Assembly.Load)
            Namespaces=
                ["system"
                 "system.collections"
                 "system.collections.generic"
                 "system.numerics"] }

