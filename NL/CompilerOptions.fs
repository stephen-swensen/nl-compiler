namespace Swensen.NL
open Swensen.NL.Ail

open System

//type CompilerTarget =
//    | Eval
//    | Nli
//    | Nlc
//    | NA

type CompilerOptions = 
    { 
        Optimize:bool 
        SemanticEnvironment:SemanticEnvironment 
        ConsoleLogging:bool
        //Target:CompilerTarget
    }
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
module CompilerOptions =
    let Default = 
        { 
            Optimize=true
            SemanticEnvironment = SemanticEnvironment.Default
            ConsoleLogging=false
        }