namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing
open System.Diagnostics

type ErrorLevel =
    | Error
    | Warning

type ErrorType =
    | Syntactic
    | Semantic
    | Internal

type CompilerError(errorRange:PositionRange, errorType:ErrorType, errorLevel:ErrorLevel, errorCode:int, msg:string, stackTrace:StackTrace) =
    member __.Type = errorType
    member __.Range = errorRange
    member __.Level = errorLevel
    member __.Message = msg
    ///The filename corresponding to Range.Start
    member __.Filename = errorRange.FileName
    member __.Code = errorCode
    member __.CodeName =
        match errorCode with
        | -1 -> "-1"
        | _ ->
            let errorCodeString = errorCode.ToString()
            let leadingZeros = //following F#'s lead here, but kinda thinking no need for leading zeros or even leading "FS" (perhaps makes searching on google easier?)
                match errorCodeString.Length with
                | 1 -> "000"
                | 2 -> "00"
                | 3 -> "0"
                | 4 -> ""
                | _ -> failwith "error code out of range: %i" errorCode
            sprintf "NL%s%i" leadingZeros errorCode
    member __.StackTrace = stackTrace
    override this.ToString() =
        let posMsg = 
            if errorRange.StartLine = errorRange.EndLine && errorRange.StartColumn = errorRange.EndColumn then
                sprintf "at Line %i, Column %i" errorRange.StartLine errorRange.StartColumn
            else
                sprintf "from Line %i, Column %i to Line %i, Column %i"  errorRange.StartLine errorRange.StartColumn  errorRange.EndLine errorRange.EndColumn


        sprintf "%A %s (%s) %s%s%s"
            errorType
            ((sprintf "%A" errorLevel).ToLower())
            this.CodeName
            posMsg
            (if String.IsNullOrWhiteSpace this.Filename then "" else " in " + this.Filename)
            (if String.IsNullOrWhiteSpace msg then "" else ": " + msg)
