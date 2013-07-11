namespace Swensen.NL

open System
open Swensen.NL
open Swensen.NL.Ail
open Swensen.NL.Ast

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

module CM = CompilerMessages
module SA = SemanticAnalysis

module FrontEnd =
    ///Default code position offset used for calculating error position info: line number, column number, and absolute offset 
    //where line number and column number start counting at 1, and absolute offset starts counting at 0.
    let DefaultOffset = (1,1,0)
    let initLexBufferWith (lineNum, colNum, absOffset) code = 
        let lexbuf = LexBuffer<char>.FromString(code)
        lexbuf.EndPos <- 
            { 
                pos_bol = absOffset-(colNum-1)
                pos_fname=""
                pos_cnum=absOffset
                pos_lnum=lineNum 
            }
        lexbuf

    let initLexBuffer code =
        initLexBufferWith DefaultOffset

    let private parse parser (lexbuf:LexBuffer<char>) r =
        try
            parser Lexer.tokenize lexbuf
        with
        | e when e.Message = "parse error" -> 
            CM.Parse_error (PositionRange(lexbuf.StartPos,lexbuf.EndPos))
            r
        | e ->
            CM.Internal_error (PositionRange(lexbuf.StartPos,lexbuf.EndPos)) (e.ToString())
            r
    
    let private semant semantf r =
        try 
            semantf()
        with
        | CompilerInterruptException ->
            r        

    let parseEval lexbuf = 
        parse Parser.parseEval lexbuf SynExpr.Nop

    let parseNli lexbuf = 
        parse Parser.parseNli lexbuf [SynStmt.Do(SynExpr.Nop)]

    let semantExprWith env expr = 
        semant (fun () -> SA.semantExprWith env expr) (ILExpr.Error(typeof<obj>))
        
    let semantExpr = semantExprWith SemanticEnvironment.Default

    let semantStmtsWith env expr = 
        semant (fun () -> SA.semantStmtsWith env expr) [ILStmt.Do(ILExpr.Error(typeof<obj>))]
        
    let semantStmts = semantStmtsWith SemanticEnvironment.Default

    let lexParseAndSemantExprWith offset env code =
        initLexBuffer offset code
        |> parseEval
        |> semantExprWith env

    let lexParseAndSemantExpr code = lexParseAndSemantExprWith DefaultOffset SemanticEnvironment.Default code

    let lexParseAndSemantStmtsWith offset env code =
        initLexBufferWith offset code
        |> parseNli
        |> semantStmtsWith env

    let lexParseAndSemantStmts code = lexParseAndSemantStmtsWith DefaultOffset SemanticEnvironment.Default code