namespace Swensen.NL

open System
open Swensen.NL
open Swensen.NL.Ail
open Swensen.NL.Ast

open FSharp.Text.Lexing
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
                //TODO: not sure what this is
                pos_orig_lnum=lineNum
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

    let parseExpr lexbuf =
        parse Parser.parseExpr lexbuf SynExpr.Nop

    let parseStmts lexbuf =
        parse Parser.parseStmts lexbuf [SynStmt.Do(SynExpr.Nop)]

    let lexParseAndSemantExprWith offset env code =
        initLexBuffer offset code
        |> parseExpr
        |> SA.semantExprWith env

    let lexParseAndSemantExpr code = lexParseAndSemantExprWith DefaultOffset SemanticEnvironment.Default code

    let lexParseAndSemantStmtsWith offset env code =
        initLexBufferWith offset code
        |> parseStmts
        |> SA.semantStmtsWith env

    let lexParseAndSemantStmts code = lexParseAndSemantStmtsWith DefaultOffset SemanticEnvironment.Default code