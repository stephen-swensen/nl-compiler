namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

type tnl =
    | Exp              of texp
    | Stmt             of tstmt
    | StmtList         of tstmt list
    //Nlc