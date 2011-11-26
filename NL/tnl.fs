namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

//represents a semantically checked top level language element
type tnl =
    | Exp               of texp
    | StmtList          of tstmt list
    | Error
    //Nlc