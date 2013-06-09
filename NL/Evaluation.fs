module Swensen.NL.Evaluation
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

type EL = MessageLogger

open Compilation

///Evaluate an NL code string using the default environment.
///If one or more compiler errors occur, then an EvaluationException is throw which contains the list of errors. Warnings are ignored.
let tryEvalWith<'a> options code : 'a option * CompilerMessage[] = 
    use sink = new BasicMessageSink(options.ConsoleLogging)
    ///Create a dynamic method from a typed expression using the default environment
    let mkDm (ilExpr:ILExpr) =
        let retTy = if isVoidOrEscapeTy ilExpr.Type then typeof<Void> else ilExpr.Type
        let dm = DynamicMethod("Eval", retTy, null)
        let il = dm.GetILGenerator() |> SmartILGenerator.fromILGenerator
        
        Emission.emit options.Optimize il ilExpr
        il.Emit(OpCodes.Ret)
        dm

    let ilTopLevel = lexParseAndSemantWith options.SemanticEnvironment Compilation.DefaultOffset code 
    if sink.HasErrors then
        None, sink.GetMessages()
    else
        let ilTopLevel = if options.Optimize then Optimization.optimize ilTopLevel else ilTopLevel
    
        match ilTopLevel.NormalizedExpr with
        | None -> 
            CompilerMessages.Could_not_normalize_eval_fragment (sprintf "%A" ilTopLevel)
            None, sink.GetMessages()
        | Some(ilExpr) ->
            let dm = mkDm ilExpr
            let value = dm.Invoke(null,null) 
            let expectedResultType = typeof<'a>
            if value = null && typeof<ValueType>.IsAssignableFrom(expectedResultType) then
                raise (invalidArg "'a" (sprintf "null eval result cannot be cast to ValueType '%s'" expectedResultType.Name))
            else
                let retval = value|> unbox |> Some
                retval, sink.GetMessages()

let tryEval<'a> = tryEvalWith<'a> CompilerOptions.Default

let evalWith<'a> options code : 'a =
    match tryEvalWith options code with
    | Some(x), _ -> x
    | None, msgs -> raise <| EvaluationException(msgs)

let eval<'a> code : 'a =
    match tryEval code with
    | Some(x), _ -> x
    | None, msgs -> raise <| EvaluationException(msgs)