module Swensen.NewLang.InteractiveCompiler
open Swensen.NewLang
open System

let rec loop() =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | str ->
        try
            printfn "%A" (Compilation.eval str)
        with
        | EvaluationException(errors) ->
            printfn "%A" errors
        | e -> printfn "%A" e

        loop()

[<EntryPoint>]
let main(args:string[]) =
    printfn "NL Interactive"
    loop()
    0