module Swensen.NL.FrontEnd

open System
open Swensen.NL
open Swensen.NL.Ail
open Swensen.NL.Ast

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

module CM = CompilerMessages
module SA = SemanticAnalysis

///Default code position offset used for calculating error position info: line number, column number, and absolute offset 
//where line number and column number start counting at 1, and absolute offset starts counting at 0.
let DefaultOffset = (1,1,0)

let lexWith (lineNum, colNum, absOffset) code =
    let lexbuf = LexBuffer<char>.FromString(code)
    lexbuf.EndPos <- 
        { 
            pos_bol = absOffset-(colNum-1)
            pos_fname=""
            pos_cnum=absOffset
            pos_lnum=lineNum 
        }
    Lexer.tokenize, lexbuf

let lex = lexWith DefaultOffset

let parseExpr (tokenize:LexBuffer<char>->token) lexbuf = 
    try
        Parser.parseExpr tokenize lexbuf
    with
    | e when e.Message = "parse error" -> 
        CM.Parse_error (PositionRange(lexbuf.StartPos,lexbuf.EndPos))
        SynExpr.Nop
    | e ->
        CM.Internal_error (PositionRange(lexbuf.StartPos,lexbuf.EndPos)) (e.ToString())
        SynExpr.Nop

let parseStmts (tokenize:LexBuffer<char>->token) lexbuf = 
    try
        Parser.parseStmts tokenize lexbuf
    with
    | e when e.Message = "parse error" -> 
        CM.Parse_error (PositionRange(lexbuf.StartPos,lexbuf.EndPos))
        [SynStmt.Do(SynExpr.Nop)]
    | e ->
        CM.Internal_error (PositionRange(lexbuf.StartPos,lexbuf.EndPos)) (e.ToString())
        [SynStmt.Do(SynExpr.Nop)]

let semantExprWith env expr = 
    try 
        SA.semantExprWith env expr
    with
    | CompilerInterruptException ->
        ILExpr.Error(typeof<obj>)
    
let semantExpr = semantExprWith SemanticEnvironment.Default

let semantStmtsWith env expr = 
    try 
        SA.semantStmtsWith env expr
    with
    | CompilerInterruptException ->
        [ILStmt.Do(ILExpr.Error(typeof<obj>))]
    
let semantStmts = semantStmtsWith SemanticEnvironment.Default

let lexParseAndSemantExprWith offset env code =
    lexWith offset code
    ||> parseExpr
    |> semantExprWith env

let lexParseAndSemantExpr code = lexParseAndSemantExprWith DefaultOffset SemanticEnvironment.Default code

let lexParseAndSemantStmtsWith offset env code =
    lexWith offset code
    ||> parseStmts
    |> semantStmtsWith env

let lexParseAndSemantStmts code = lexParseAndSemantStmtsWith DefaultOffset SemanticEnvironment.Default code