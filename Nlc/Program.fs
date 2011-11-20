module Swensen.NL.Nlc
open Swensen.NL

[<EntryPoint>]
let main args =
    
    let fileNames, asmName =
        match args with
        | [|fileNames; asmName|] -> fileNames.Split('|'), asmName // | is an invalid path name so good delimiter
        | _ -> failwithf "Could not parse args: %A" args
    try
        Compilation.compileFromFiles fileNames asmName
        0
    with e ->
        printfn "%s" e.Message
        1

