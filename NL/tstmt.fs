namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

type tstmt =
    | Let               of string * (rexp * PositionRange) * Type
    | OpenNamespace     of (string * PositionRange)
    | OpenAssembly      of (string * PositionRange)
    | Do                of rexp * PositionRange * Type

