namespace Swensen.NL.VisualNli
open System
open Swensen.NL

//todo: move to maing NL namespace / assembly?
type NliSessionManager() =
    //data
    let mutable nli = Swensen.NL.Nli()
    let mutable errorCount = 0
    let mutable warningCount = 0
    let mutable exnCount = 0

    member __.Reset() =
        nli <- Swensen.NL.Nli()
        errorCount <- 0
        warningCount <- 0
        exnCount <- 0


    member __.Submit(code:String) = 
        let collectMessages() = [|  
            let msgs = Swensen.NL.MessageLogger.ActiveLogger.GetMessages()
            for msg in msgs do
                match msg.Level with
                | MessageLevel.Error ->
                    yield (sprintf "error%i" errorCount,  msg :> obj, msg.GetType())
                    errorCount <- errorCount + 1 
                | MessageLevel.Warning ->
                    yield (sprintf "warning%i" warningCount,  msg :> obj, msg.GetType())
                    warningCount <- warningCount + 1
        |]

        try
            match nli.TrySubmit(code) with
            | Some(results) -> [| yield! collectMessages(); yield! results |]
            | None -> collectMessages()
        with e ->
            let result = sprintf "exn%i" exnCount, e :> obj, e.GetType()
            exnCount <- exnCount + 1
            [| yield! collectMessages(); yield result |]

