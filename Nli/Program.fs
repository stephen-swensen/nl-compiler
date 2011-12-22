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
                for fName,fValue in results do
                    printfn "%s = %A" fName fValue
            | None ->
                ()
        with
        | e -> printfn "%A" e

        loop(nli)

[<EntryPoint>]
let main(args:string[]) =
    printfn "NL Interactive"
    let nli = new Nli({ CompilerOptions.Default with InstallErrorLogger=ErrorLogger.InstallConsoleLogger })
    loop(nli)
    0