﻿namespace Swensen.NewLang

open Microsoft.FSharp.Text.Lexing

//type CompilerExceptionType =
//    | Syntax
//    | Semantic
//    | Internal
//
//type PositionRange(posStart, posEnd) =
//
//type CompilerException(pos: Position, cet: CompilerExceptionType) =
//    match cet with
//    |

type SyntaxErrorException(pos: Position) =
    inherit exn(sprintf "Syntax error at line %i, column %i" pos.Line pos.Column)

type SemanticErrorException(pos: Position, msg:string) =
    inherit exn(sprintf "Semantic error at line %i, column %i: %s" pos.Line pos.Column msg)
