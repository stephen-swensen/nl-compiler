namespace Swensen.NewLang

open System
open Microsoft.FSharp.Text.Lexing

type CompilerErrorLevel =
    | Error
    | Warning

type CompilerErrorType =
    | Syntactic
    | Semantic
    | Internal
//
type CompilerErrorRange(posStart:Position, posEnd:Position) =
    member __.Start = posStart
    member __.End = posEnd

type CompilerError(errorRange:CompilerErrorRange, errorType:CompilerErrorType, errorLevel:CompilerErrorLevel, errorCode:int, msg:string, filename:string) =
    member __.Type = errorType
    member __.Range = errorRange
    member __.Level = errorLevel
    member __.Message = msg
    member __.Filename = filename
    member __.Code = errorCode
    override __.ToString() =
        let posMsg = 
            if errorRange.Start = errorRange.End then
                sprintf "at line %i, column %i" errorRange.Start.Line errorRange.Start.Column
            else
                sprintf "from line %i, column %i to line %i, column %i"  errorRange.Start.Line errorRange.Start.Column  errorRange.End.Line errorRange.End.Column

        sprintf "%A %s (%i) %s%s%s"
            errorType
            ((sprintf "%A" errorLevel).ToLower())
            errorCode
            posMsg
            (if String.IsNullOrWhiteSpace filename then "" else " in " + filename)
            (if String.IsNullOrWhiteSpace msg then "" else ": " + msg)


type CompilerException(ce: CompilerError) =
    inherit exn(ce.ToString())
    member __.Info = ce

type SyntaxErrorException(pos: Position) =
    inherit exn(sprintf "Syntax error at line %i, column %i" pos.Line pos.Column)

type SemanticErrorException(pos: Position, msg:string) =
    inherit exn(sprintf "Semantic error at line %i, column %i: %s" pos.Line pos.Column msg)