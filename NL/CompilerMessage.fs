namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing
open System.Diagnostics

type MessageLevel =
    | Error
    | Warning

type MessageType =
    | Syntactic
    | Semantic
    | Internal

///note: leave stackTrace as null in ReleaseMode
type CompilerMessage(msgRange:PositionRange, msgType:MessageType, msgLevel:MessageLevel, msgCode:int, msg:string, stackTrace:StackTrace) =
    member __.Type = msgType
    member __.Range = msgRange
    member __.Level = msgLevel
    member __.Message = msg
    ///The filename corresponding to Range.Start
    member __.Filename = msgRange.FileName
    ///The unique message coe identifier
    member __.Code = msgCode
    ///The unique message code identifier in as a searchable string
    member __.CodeName =
        match msgCode with
        | -1 -> "-1"
        | _ ->
            let msgCodeString = msgCode.ToString()
            let leadingZeros = //following F#'s lead here, but kinda thinking no need for leading zeros or even leading "FS" (perhaps makes searching on google easier?)
                match msgCodeString.Length with
                | 1 -> "000"
                | 2 -> "00"
                | 3 -> "0"
                | 4 -> ""
                | _ -> failwith "error code out of range: %i" msgCode
            sprintf "NL%s%i" leadingZeros msgCode
    
    #if DEBUG
    member __.StackTrace = stackTrace
    #endif

    override this.ToString() =
        let posMsg = 
            if msgRange.StartLine = msgRange.EndLine && msgRange.StartColumn = msgRange.EndColumn then
                sprintf "at Line %i, Column %i" msgRange.StartLine msgRange.StartColumn
            else
                sprintf "from Line %i, Column %i to Line %i, Column %i"  msgRange.StartLine msgRange.StartColumn  msgRange.EndLine msgRange.EndColumn


        sprintf "%A %s (%s) %s%s%s"
            msgType
            ((sprintf "%A" msgLevel).ToLower())
            this.CodeName
            posMsg
            (if String.IsNullOrWhiteSpace this.Filename then "" else " in " + this.Filename)
            (if String.IsNullOrWhiteSpace msg then "" else ": " + msg)
