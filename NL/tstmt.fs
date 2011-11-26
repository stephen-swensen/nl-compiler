namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

type tstmt =
    //variable stmt
    | Let               of string * texp
    //expression stmt
    | Do                of texp