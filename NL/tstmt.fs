namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

type tstmt =
    //variable stmt
    | Let               of string * rexp * Type
    //expression stmt
    | Do                of rexp * Type

