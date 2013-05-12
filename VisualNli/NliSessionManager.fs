namespace Swensen.NL.VisualNli
open System
open Swensen.NL

//todo: move to maing NL namespace / assembly?
type NliSessionManager() as this =
    //data
    [<DefaultValue>] val mutable nli : Nli
    [<DefaultValue>] val mutable errorCount : int
    [<DefaultValue>] val mutable warningCount :int
    [<DefaultValue>] val mutable exnCount :int

    do this.Reset()

    member this.Reset() =
        this.nli <- Swensen.NL.Nli({ CompilerOptions.Default with MessageLoggerInstaller=MessageLogger.InstallConsoleLogger })
        this.errorCount <- 0
        this.warningCount <- 0
        this.exnCount <- 0


    member this.Submit(code:String) = 
        let collectMessages() = [|  
            let msgs = Swensen.NL.MessageLogger.ActiveLogger.GetMessages()
            for msg in msgs do
                match msg.Level with
                | MessageLevel.Error ->
                    yield (sprintf "error%i" this.errorCount,  msg :> obj, msg.GetType())
                    this.errorCount <- this.errorCount + 1 
                | MessageLevel.Warning ->
                    yield (sprintf "warning%i" this.warningCount,  msg :> obj, msg.GetType())
                    this.warningCount <- this.warningCount + 1
        |]

        try
            match this.nli.TrySubmit(code) with
            | Some(results) -> [| yield! collectMessages(); yield! results |]
            | None -> collectMessages()
        with e ->
            Console.WriteLine(e.ToString())
            let result = sprintf "exn%i" this.exnCount, e :> obj, e.GetType()
            this.exnCount <- this.exnCount + 1
            [| yield! collectMessages(); yield result |]

