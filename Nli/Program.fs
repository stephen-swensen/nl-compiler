module Swensen.NL.Nli
open Swensen.NL
open System


let rec loop(nli:Nli) =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | code ->
        try
            match nli.TrySubmit(code) with
            | Some(results) ->
                for fName,fValue,fTy in results do
                    printfn "[%s] %s = %A" fTy.Name fName fValue
            | None ->
                ()
        with
        | e -> printfn "%A" e

        loop(nli)

[<EntryPoint>]
let main(args:string[]) =
    //Console.OutputEncoding <- System.Text.Encoding.Unicode
    //Unicode (UTF16) doesn't work, so will use UTF8, which means we will loose combining chars
    Console.OutputEncoding <- System.Text.Encoding.UTF8 

    printfn "NL Interactive"
    let nli = new Nli({ CompilerOptions.Default with MessageLoggerInstaller=MessageLogger.InstallConsoleLogger })
    loop(nli)
    0