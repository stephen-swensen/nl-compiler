module Swensen.NL.Nlc
open Swensen.NL

[<EntryPoint>]
let main args =
    let fileNames, asmName =
        match args with
        | [|fileNames; asmName|] -> fileNames.Split('|'), asmName // | is an invalid path name so good delimiter
        | _ -> failwithf "Could not parse args: %A\n\nSyntax is nlc \"filename1.nl|filename2.nl|...\" \"assembly name\"" args
    try
        
        Compilation.compileFromFiles fileNames asmName
        0
    with e ->
        printfn "%s" e.Message
        1

