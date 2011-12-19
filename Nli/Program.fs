module Swensen.NL.Nli
open Swensen.NL
open System


let rec loop(nli:Compilation.Nli) =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | code ->
        try
            let results = nli.Submit(code)
            for fName,fValue in results do
                printfn "%s = %A" fName fValue
        with
        | :? EvaluationException as e ->
            printfn "%A" e.Errors
        | e -> printfn "%A" e

        loop(nli)

[<EntryPoint>]
let main(args:string[]) =
    printfn "NL Interactive"
    let nli = new Compilation.Nli()
    loop(nli)
    0