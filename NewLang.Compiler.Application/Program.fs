module Swensen.NewLang.Compiler.Application.Program
open Swensen.NewLang

[<EntryPoint>]
let main [|sourceFileName; asmName; asmFileName;|] =
    Compiler.compile (System.IO.File.ReadAllText sourceFileName |> Compiler.parseFromString) asmName asmFileName
    0

