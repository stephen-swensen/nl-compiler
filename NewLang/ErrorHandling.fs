namespace Swensen.NewLang

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

type PositionRange(posStart:Position, posEnd:Position) =
//    new(posRangeStart:PositionRange,posRangeEnd:PositionRange) = 
//        new PositionRange(posRangeStart.Start, posRangeEnd.End)
    member __.StartLine = posStart.Line
    member __.EndLine = posEnd.Line
    member __.StartColumn = posStart.Column
    member __.EndColumn = posEnd.Column-1
    //assume error cannot span more than one file
    member __.FileName = posStart.FileName
    

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
            let leadingZeros = 
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

[<AllowNullLiteral>]
type ErrorLogger() =
    let errors = System.Collections.Generic.List<CompilerError>()
    let mutable errorCount = 0
    let mutable warningCount = 0
    ///A readonly view of all the loggged errors
    member __.Errors = seq { yield! errors }

    abstract Log : CompilerError -> unit
    ///Log the compiler error
    default __.Log(ce:CompilerError) =
        match ce.Level with
        | ErrorLevel.Error -> errorCount <- errorCount + 1
        | ErrorLevel.Warning -> warningCount <- warningCount + 1

        errors.Add(ce)

//    abstract Clear : unit -> unit
//    //clear the active logger
//    default __.Clear() = 
//        errors.Clear()
//        errorCount <- 0
//        warningCount <- 0


    ///The count of the number of compiler errors which have "Error" severity level
    member __.ErrorCount = errorCount
    ///The count of the number of compiler errors which have "Warning" severity level
    member __.WarningCount = warningCount
    
    //based on https://github.com/fsharp/fsharp/blob/master/src/fsharp/ErrorLogger.fs
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private activeLogger : ErrorLogger

    ///The thread static active error logger; if not set, the default error logger is installed
    static member ActiveLogger
        with get() = 
            if ErrorLogger.activeLogger = null then
               ErrorLogger.InstallDefaultLogger()
            ErrorLogger.activeLogger
        and set(v) = 
            ErrorLogger.activeLogger <- v

    static member InstallDefaultLogger() =
        ErrorLogger.activeLogger <- ErrorLogger()

    static member InstallConsoleLogger() =
        ErrorLogger.activeLogger <- ConsoleErrorLogger()

and ConsoleErrorLogger() = 
    inherit ErrorLogger()
    override  __.Log(ce:CompilerError) =
        base.Log(ce)
        let writer =
            match ce.Level with
            | ErrorLevel.Error -> stderr
            | ErrorLevel.Warning -> stdout
        
        writer.WriteLine(sprintf "|%s|" (ce.ToString()))

//error messages may be inspired and or copied entirely from C# and F#
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ErrorMessage = 
    ///this function allows us to build a function which produces a ErrorMessage from the format string parameters
    let private mk errorLevel errorType code  (pos:PositionRange) (f:Printf.StringFormat<_,unit>) =
        Printf.ksprintf (fun s -> ErrorLogger.ActiveLogger.Log (CompilerError(pos, errorType, errorLevel, code, s, StackTrace()))) f
    
    //error code -1 stands for unspecified error
    
    let Could_not_resolve_type pos = 
        mk ErrorLevel.Error ErrorType.Semantic 1 pos "Could not resolve type '%s'"
    
    let Could_not_resolve_types pos = 
        mk ErrorLevel.Error ErrorType.Semantic 2 pos "Could not resolve types: '%s'"

    let No_overload_found_for_binary_operator pos = 
        mk ErrorLevel.Error ErrorType.Semantic 3 pos "Binary operator '%s' cannot be applied to operands of type '%s' and '%s'"
    
    let Variable_set_type_mismatch pos = //todo: rewrite to make clearer
        mk ErrorLevel.Error ErrorType.Semantic 4 pos "Type mismatch: variable '%s' of type '%s' cannot be assigned a value of the different type '%s'"

    let Variable_not_found pos = 
        mk ErrorLevel.Error ErrorType.Semantic 5 pos "Variable '%s' not found"

    let Expected_type_but_got_type pos = 
        mk ErrorLevel.Error ErrorType.Semantic 6 pos "Expected type '%s' but got type '%s'"

    let Break_outside_of_loop pos = 
        mk ErrorLevel.Error ErrorType.Semantic 8 pos "'break()' is only valid inside a loop body"

    let Continue_outside_of_loop pos = 
        mk ErrorLevel.Error ErrorType.Semantic 9 pos "'continue()' is only valid inside a loop body"

    let Invalid_instance_method pos = 
        mk ErrorLevel.Error ErrorType.Semantic 10 pos "Not a valid instance method '%s' for the given instance type '%s' with argument types %s" //final array of types is sprinted externally

    let Invalid_static_method pos = 
        mk ErrorLevel.Error ErrorType.Semantic 11 pos "Not a valid static method '%s' on the type '%s' with argument types %s" //final array of types is sprinted externally

    let Null_is_invalid_for_value_types pos = 
        mk ErrorLevel.Error ErrorType.Semantic 12 pos "'null' is not a valid value for the value type '%s'"

    //todo: consider adding "variable not found" possiblity to all of this too, e.g. "x.toString()" trips this error.
    let Could_not_resolve_possible_method_call_or_contructor_type pos = 
        mk ErrorLevel.Error ErrorType.Semantic 13 pos "Could not resolve possible method call type '%s' or constructor type '%s'"

    let Void_cannot_be_instantiated pos = 
        mk ErrorLevel.Error ErrorType.Semantic 14 pos "'System.Void' cannot be instantiated"

    let Could_not_resolve_constructor pos = 
        mk ErrorLevel.Error ErrorType.Semantic 15 pos "Could not resolve constructor for type '%s' with argument types %s"

    let Void_invalid_in_let_binding pos = 
        mk ErrorLevel.Error ErrorType.Semantic 16 pos "'System.Void' is not a valid assignment value in a let binding"

    let Unreachable_code_detected pos = 
        mk ErrorLevel.Error ErrorType.Semantic 17 pos "Unreachable code detected"

    let Namespace_not_found pos = 
        mk ErrorLevel.Error ErrorType.Semantic 18 pos "Namespace '%s' does not exist in any currently open assemblies %s"

    let Could_not_resolve_assembly pos = 
        mk ErrorLevel.Error ErrorType.Semantic 19 pos "Could not resolve assembly reference '%s'"

    let Casting_to_void_invalid pos = 
        mk ErrorLevel.Error ErrorType.Semantic 20 pos "Casting to System.Void will always fail"

    let Casting_noop pos = 
        mk ErrorLevel.Error ErrorType.Semantic 21 pos "Casting a value to its own type '%s' is a no-op"

    let Casting_from_type_to_type_always_invalid pos =
        mk ErrorLevel.Error ErrorType.Semantic 22 pos "Cast from type '%s' to type '%s' will always fail"

    let IfThenElse_branch_type_mismatch pos =
        mk ErrorLevel.Error ErrorType.Semantic 23 pos "'if ... then' and 'else' branches must be of same type but instead are '%s' and '%s'"

    let Internal_error pos =
        mk ErrorLevel.Error ErrorType.Internal 24 pos "%s"

    let No_overload_found_for_unary_operator pos = 
        mk ErrorLevel.Error ErrorType.Semantic 25 pos "Unary operator '%s' cannot be applied to operand of type '%s'"

    let Int32_literal_out_of_range pos = 
        mk ErrorLevel.Error ErrorType.Semantic 26 pos "'System.Int32' literal must be between %i and %i but is %s" Int32.MinValue Int32.MaxValue

    let Double_literal_out_of_range pos = 
        mk ErrorLevel.Error ErrorType.Semantic 27 pos "'System.Double' literal must be between %f and %f but is %s" Double.MinValue Double.MaxValue

///Use this exception to interrupt local compiler work due to unrecoverable errors (don't actually consider this an error though)
exception CompilerInterruptException
exception EvaluationException of CompilerError[]
    with 
        member this.Errors = this.Data0

exception CompilerInternalErrorException