module Swensen.NewLang.InteractiveCompiler
open Swensen.NewLang
open System

let rec loop() =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | str ->
        try
            printfn "%A" (Compiler.eval str)
        with
        | :? SemanticErrorException as e -> printfn "%s" e.Message
        | :? SyntaxErrorException as e -> printfn "%s" e.Message
        | e -> printfn "%A" e

        loop()

[<EntryPoint>]
let main(args:string[]) =
    printfn "NL Interactive"
    loop()
    0