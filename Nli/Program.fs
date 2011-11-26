module Swensen.NL.Nli
open Swensen.NL
open System

let rec loop() =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | str ->
        try
            printfn "%A" (Compilation.eval str)
        with
        | :? EvaluationException as e ->
            printfn "%A" e.Errors
        | e -> printfn "%A" e

        loop()

[<EntryPoint>]
let main(args:string[]) =
    printfn "NL Interactive"
    loop()
    0