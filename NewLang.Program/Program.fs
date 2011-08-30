open Swensen.NewLang
open System

let rec loop() =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | str ->
        try
            printfn "%A" (Compiler.eval str)
        with ex ->
            let ex = if ex.InnerException <> null then ex.InnerException else ex
            printfn "%s" ex.Message

        //printfn "%A" (Compiler.eval str)
        loop()

[<EntryPoint>]
let main(args:string[]) =
    printfn "===NewLangulator==="
    printfn @"Enter an equation and press ENTER, or enter ""exit"" and press ENTER to exit."
    loop()
    0