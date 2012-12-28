﻿namespace Swensen.NL

open System
///The base, in-memory message logger. Not thread-safe.
[<AllowNullLiteral>]
type MessageLogger() =
    let messages = System.Collections.Generic.List<CompilerMessage>()
    let mutable errorCount = 0
    let mutable warningCount = 0
    
    ///Get a snapshot of all the logged errors
    member __.GetMessages() = messages.ToArray()
    ///Get a snapshot of all the logged errors with the given level
    member __.GetMessages(levelFilter) = messages |> Seq.filter (fun msg -> msg.Level |> levelFilter) |> Seq.toArray

    abstract Log : CompilerMessage -> unit
    ///Log the compiler error
    default __.Log(ce:CompilerMessage) =
        match ce.Level with
        | MessageLevel.Error -> errorCount <- errorCount + 1
        | MessageLevel.Warning -> warningCount <- warningCount + 1

        messages.Add(ce)

    ///Indicates whether there are any errors ("Error" severity level) that have been logged
    member __.HasErrors = errorCount > 0

    ///Indicates whether there are any warnings ("Warning" severity level) that have been logged
    member __.HasWarnings = warningCount > 0

    ///The count of the number of compiler messages which have "Error" severity level
    member __.ErrorCount = errorCount
    ///The count of the number of compiler messages which have "Warning" severity level
    member __.WarningCount = warningCount
    
    //based on https://github.com/fsharp/fsharp/blob/master/src/fsharp/ErrorLogger.fs
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private activeLogger : MessageLogger

    ///The thread static active error logger; if not set, the default error logger is installed
    static member ActiveLogger
        with get() = 
            if MessageLogger.activeLogger = null then
               MessageLogger.activeLogger <- MessageLogger()
            MessageLogger.activeLogger
        and set(v) = 
            MessageLogger.activeLogger <- v

    static member InstallInMemoryLogger = fun () ->
        MessageLogger.activeLogger <- new MessageLogger()

    static member InstallConsoleLogger = fun () ->
        MessageLogger.activeLogger <- new ConsoleMessageLogger()
    
///Maybe make MessageLogger event driven instead of inheritence driven (i.e.
///have a "ErrorLogged" event which can have a "ConsoleListener" event attached
///if desired).
and ConsoleMessageLogger() = 
    inherit MessageLogger()
    override  __.Log(ce:CompilerMessage) =
        base.Log(ce)
        let writer =
            match ce.Level with
            | MessageLevel.Error -> stderr
            | MessageLevel.Warning -> stdout
        
        writer.WriteLine(sprintf "|%s|" (ce.ToString()))