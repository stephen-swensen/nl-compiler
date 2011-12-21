module Swensen.NL.Evaluation
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

type EL = ErrorLogger

open Compilation

//should have "tryEval" which doesn't throw?

///Evaluate an NL code string using the default environment.
///If one or more compiler errors occur, then an EvaluationException is throw which contains the list of errors. Warnings are ignored.
let evalWith<'a> options code : 'a = 
    options.InstallErrorLogger()

    ///Create a dynamic method from a typed expression using the default environment
    let mkDm (ilExpr:ILExpr) =
        let dm = DynamicMethod("Eval", ilExpr.Type, null)
        let il = dm.GetILGenerator()
        Emission.emit il ilExpr
        il.Emit(OpCodes.Ret)
        dm

    let ilTopLevel = lexParseAndSemantWith options.SemanticEnvironment code 
    if EL.ActiveLogger.HasErrors then
        raise <| EvaluationException(EL.ActiveLogger.GetErrors())

    let ilTopLevel = if options.Optimize then Optimization.optimize ilTopLevel else ilTopLevel
    
    let ilExpr =
        match ilTopLevel.NormalizedExpr with
        | Some(x) -> x
        | None -> raise <| EvaluationException(sprintf "NL fragment could not be normalized for evaluation: %A" ilTopLevel)

    let dm = mkDm ilExpr
    dm.Invoke(null,null) |> unbox


let eval<'a> = evalWith<'a> CompilerOptions.Default