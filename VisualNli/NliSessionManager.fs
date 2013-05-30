namespace Swensen.NL.VisualNli
open System
open Swensen.NL

type SubmissionStats = { ErrorCount:int; WarningCount:int; Time:int64}

//todo: move to maing NL namespace / assembly?
type NliSessionManager() as this =
    //data
    [<DefaultValue>] val mutable nli : Nli
    [<DefaultValue>] val mutable errorCount : int
    [<DefaultValue>] val mutable warningCount :int
    [<DefaultValue>] val mutable exnCount :int

    do this.Reset()

    member this.Reset() =
        this.nli <- Swensen.NL.Nli({ CompilerOptions.Default with InstallMessageLogger=MessageLogger.InstallConsoleLogger })
        this.errorCount <- 0
        this.warningCount <- 0
        this.exnCount <- 0

    member this.Submit(code:String) =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let results =
            try
                match this.nli.TrySubmit(code) with
                | Some(results) -> [| yield! results |]
                | None -> [||]
            with e ->
                Console.WriteLine(e.ToString())
                let result = sprintf "exn%i" this.exnCount, e :> obj, e.GetType()
                this.exnCount <- this.exnCount + 1
                [| yield result |]
        sw.Stop()
        let time = sw.ElapsedMilliseconds

        let errorCountOffset = this.errorCount
        let warningCountOffset = this.warningCount
        let messages = [|  
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
        let stats = { ErrorCount=this.errorCount-errorCountOffset; WarningCount=this.warningCount-warningCountOffset; Time=time }
        (stats, [|yield! messages; yield! results|])