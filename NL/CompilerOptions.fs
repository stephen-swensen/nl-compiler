namespace Swensen.NL
open Swensen.NL.Ail

open System

type CompilerOptions = { Optimize:bool ; SemanticEnvironment:SemanticEnvironment }
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
module CompilerOptions =
    let Default = { Optimize=true ; SemanticEnvironment=SemanticEnvironment.Default }