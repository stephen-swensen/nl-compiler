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
        InstallMessageLogger: unit -> unit
        //Target:CompilerTarget
    }
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
module CompilerOptions =
    let Default = 
        { 
            Optimize=true
            SemanticEnvironment = SemanticEnvironment.Default
            InstallMessageLogger = MessageLogger.InstallInMemoryLogger
        }