namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

type rnl =
    | Exp              of rexp
    | Stmt             of rstmt
    | StmtList         of rstmt list