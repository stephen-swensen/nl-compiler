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

type PositionRange(posStart:Position, posEnd:Position) =
    member __.Start = posStart
    member __.End = posEnd

type CompilerError(errorRange:PositionRange, errorType:CompilerErrorType, errorLevel:CompilerErrorLevel, errorCode:int, msg:string) =
    member __.Type = errorType
    member __.Range = errorRange
    member __.Level = errorLevel
    member __.Message = msg
    ///The filename corresponding to Range.Start
    member __.Filename = errorRange.Start.FileName
    member __.Code = errorCode
    override this.ToString() =
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
            (if String.IsNullOrWhiteSpace this.Filename then "" else " in " + this.Filename)
            (if String.IsNullOrWhiteSpace msg then "" else ": " + msg)


type CompilerException(ce: CompilerError) =
    inherit exn(ce.ToString())
    member __.CompilerError = ce

//todo: eventually remove this, we don't want to throw exceptions on error, rather gather as 
//many errors as we can (using error correction) and report back at the end of semantic analysis.
exception SemanticAnalysisException of PositionRange * string