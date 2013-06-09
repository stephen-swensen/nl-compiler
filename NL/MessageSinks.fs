namespace Swensen.NL

open System

type internal MessageLogger =
    //based on https://github.com/fsharp/fsharp/blob/master/src/fsharp/ErrorLogger.fs
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private messageLogged : Event<CompilerMessage>

    ///The thread static event triggered by calls to static CompilerMessage.Log. This event should typically not be
    ///subscribed to directly, instead, use a subclass of AbstractMessageSink for automatic subscribe on init and unsubscribe
    ///on Dispose.
    static member MessageLogged
        with get() = 
            if obj.ReferenceEquals(MessageLogger.messageLogged, null) then
               MessageLogger.messageLogged <- new Event<CompilerMessage>()
            MessageLogger.messageLogged.Publish

    ///Triggers the MessageLogged event.
    static member Log(msg) =
        MessageLogger.messageLogged.Trigger(msg)

///Abstract message sink which subscribes to MessageLogger.MessageLogged on initialization and unsubscribes when disposed.
[<AbstractClass>]
type AbstractMessageSink() as this =
    let sinkHandle = MessageLogger.MessageLogged.Subscribe(this.Sink)
    ///Sink a log message. This method should almost never be used directly (it is invoked by the MessageLogged event).
    abstract Sink : CompilerMessage -> unit
    interface IDisposable with
        override __.Dispose() = sinkHandle.Dispose()

///A basic stateful message sink (messages are accumulated in an internal list, which can be queried / retrieved at any time)
///with optional console logging (default is false). This message sink is typically used for internal log orchestration.
type BasicMessageSink(?consoleLogging:bool) =
    inherit AbstractMessageSink()

    let consoleLogging = defaultArg consoleLogging false
    let logToConsole (msg:CompilerMessage) = 
        let writer =
            match msg.Level with
            | MessageLevel.Error -> stderr
            | MessageLevel.Warning -> stdout
        
        writer.WriteLine(sprintf "|%s|" (msg.ToString()))

    let messages = System.Collections.Generic.List<CompilerMessage>()
    let mutable errorCount = 0
    let mutable warningCount = 0
    
    ///Get a snapshot of all the logged errors
    member __.GetMessages() = messages.ToArray()
    ///Get a snapshot of all the logged errors with the given level
    member __.GetMessages(levelFilter) = messages |> Seq.filter (fun msg -> msg.Level |> levelFilter) |> Seq.toArray

    ///Sink the compiler message (this method should almost never be used directly)
    override __.Sink(msg:CompilerMessage) =
        match msg.Level with
        | MessageLevel.Error -> errorCount <- errorCount + 1
        | MessageLevel.Warning -> warningCount <- warningCount + 1

        messages.Add(msg)
        if consoleLogging then
            logToConsole msg

    ///Indicates whether there are any errors ("Error" severity level) that have been logged
    member __.HasErrors = errorCount > 0

    ///Indicates whether there are any warnings ("Warning" severity level) that have been logged
    member __.HasWarnings = warningCount > 0

    ///The count of the number of compiler messages which have "Error" severity level
    member __.ErrorCount = errorCount
    ///The count of the number of compiler messages which have "Warning" severity level
    member __.WarningCount = warningCount