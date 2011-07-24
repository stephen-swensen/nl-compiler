open Swensen.Calc
open System

let rec loop() =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | str ->
        try
            printfn "%A" 
               (match Evaluator.eval str with
                | Ast.Integer x -> box x
                | Ast.Rational x -> box x)
        with ex ->
            printfn "Unhandled Exception: %s" ex.Message

        loop()

[<EntryPoint>]
let main(args:string[]) =
    printfn "===Calculator==="
    printfn @"Enter an equation and press ENTER, or enter ""exit"" and press ENTER to exit."
    loop()
    0

