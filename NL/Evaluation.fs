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

///Evaluate an NL code string using the default environment.
///If one or more compiler errors occur, then an EvaluationException is throw which contains the list of errors. Warnings are ignored.
let tryEvalWith<'a> options code : 'a option = 
    options.InstallErrorLogger()

    ///Create a dynamic method from a typed expression using the default environment
    let mkDm (ilExpr:ILExpr) =
        let dm = DynamicMethod("Eval", ilExpr.Type, null)
        let il = dm.GetILGenerator() |> SmartILGenerator.fromILGenerator
        
        Emission.emit il ilExpr
        il.Emit(OpCodes.Ret)
        dm

    let ilTopLevel = lexParseAndSemantWith options.SemanticEnvironment code 
    if EL.ActiveLogger.HasErrors then
        None
    else
        let ilTopLevel = if options.Optimize then Optimization.optimize ilTopLevel else ilTopLevel
    
        match ilTopLevel.NormalizedExpr with
        | None -> 
            ErrorMessages.Could_not_normalize_eval_fragment (sprintf "%A" ilTopLevel)
            None
        | Some(ilExpr) ->
            let dm = mkDm ilExpr
            dm.Invoke(null,null) |> unbox |> Some


let tryEval<'a> = tryEvalWith<'a> CompilerOptions.Default

let evalWith<'a> options code : 'a =
    match tryEvalWith options code with
    | Some(x) -> x
    | None -> raise <| EvaluationException()

let eval<'a> code : 'a =
    match tryEval code with
    | Some(x) -> x
    | None -> raise <| EvaluationException()