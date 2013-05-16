namespace Swensen.NL

open System
open Microsoft.FSharp.Text.Lexing

type PositionRange(posStart:Position, posEnd:Position) =
    static member Empty = PositionRange(Position.Empty, Position.Empty)
    member __.StartLine = posStart.Line
    member __.EndLine = posEnd.Line
    //lexer positions start counting at 0, we start at 1
    member __.StartColumn = posStart.Column+1
    member __.EndColumn = posEnd.Column+1
    //assume error cannot span more than one file
    member __.FileName = posStart.FileName
    member __.Start = posStart
    member __.End = posEnd
    new(posRangeStart:PositionRange, posRangeEnd:PositionRange) = 
        new PositionRange(posRangeStart.Start, posRangeEnd.End)    

    override this.Equals(other:obj) =
        match other with
        | :? PositionRange as other -> this.Start = other.Start && this.End = other.End
        | _ -> false

    override this.GetHashCode() =
        this.Start.GetHashCode() ^^^ this.End.GetHashCode()