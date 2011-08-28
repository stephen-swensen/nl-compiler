module Swensen.NewLang.Compiler.Application.Program
open Swensen.NewLang

[<EntryPoint>]
let main args =
    
    let fileNames, asmName =
        match args with
        | [|fileNames; asmName|] -> fileNames.Split('|'), asmName // | is an invalid path name so good delimiter
        | _ -> failwithf "Could not parse args: %A" args
    try
        Compiler.compileFromFiles fileNames asmName
        0
    with e ->
        printfn "%A" e
        1

