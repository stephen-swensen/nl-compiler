module Swensen.NL.Nli
open Swensen.NL
open System


let rec loop(nli:Nli) =
    printf "> "
    match Console.ReadLine() with
    | "exit" -> ()
    | code ->
        try
            let results =  nli.Submit(code)
            for fName,fValue,fTy in results do
                printfn "[%s] %s = %A" fTy.Name fName fValue
        with
        | e -> printfn "%A" e

        loop(nli)

[<EntryPoint>]
let main(args:string[]) =
    //Console.OutputEncoding <- System.Text.Encoding.Unicode
    //Unicode (UTF16) doesn't work, so will use UTF8, which means we will loose combining chars
    Console.OutputEncoding <- System.Text.Encoding.UTF8 

    printfn "NL Interactive"
    let nli = new Nli({ CompilerOptions.Default with ConsoleLogging=true })
    loop(nli)
    0