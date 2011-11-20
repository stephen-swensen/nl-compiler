namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

type rstmt =
    | Let               of string * (rexp * PositionRange)
    | OpenNamespace     of string * PositionRange
    | OpenAssembly      of string * PositionRange
    | Do                of rexp