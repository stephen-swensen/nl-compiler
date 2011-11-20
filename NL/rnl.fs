namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

//represents a "raw" top level language element
type rnl =
    | Exp              of rexp
    | StmtList         of rstmt list