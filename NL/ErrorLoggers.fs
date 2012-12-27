namespace Swensen.NL

open System
///The base, in-memory error logger. Not thread-safe.
[<AllowNullLiteral>]
type ErrorLogger() =
    let errors = System.Collections.Generic.List<CompilerError>()
    let mutable errorCount = 0
    let mutable warningCount = 0
    
    ///Get a snapshot of all the logged errors
    member __.GetErrors() = errors.ToArray()
    ///Get a snapshot of all the logged errors with the given level
    member __.GetErrors(level:ErrorLevel) = errors |> Seq.filter(fun err -> err.Level = level) |> Seq.toArray

    abstract Log : CompilerError -> unit
    ///Log the compiler error
    default __.Log(ce:CompilerError) =
        match ce.Level with
        | ErrorLevel.Error -> errorCount <- errorCount + 1
        | ErrorLevel.Warning -> warningCount <- warningCount + 1

        errors.Add(ce)

    ///Indicates whether there are any errors ("Error" severity level) that have been logged
    member __.HasErrors = errorCount > 0

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
               ErrorLogger.activeLogger <- ErrorLogger()
            ErrorLogger.activeLogger
        and set(v) = 
            ErrorLogger.activeLogger <- v

    static member InstallInMemoryLogger = fun () ->
        ErrorLogger.activeLogger <- new ErrorLogger()

    static member InstallConsoleLogger = fun () ->
        ErrorLogger.activeLogger <- new ConsoleErrorLogger()
    
///Maybe make ErrorLogger event driven instead of inheritence driven (i.e.
///have a "ErrorLogged" event which can have a "ConsoleListener" event attached
///if desired).
and ConsoleErrorLogger() = 
    inherit ErrorLogger()
    override  __.Log(ce:CompilerError) =
        base.Log(ce)
        let writer =
            match ce.Level with
            | ErrorLevel.Error -> stderr
            | ErrorLevel.Warning -> stdout
        
        writer.WriteLine(sprintf "|%s|" (ce.ToString()))
