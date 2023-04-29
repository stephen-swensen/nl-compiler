namespace Swensen.NL.VisualNli
open Swensen.NL
open Microsoft.FSharp.Text.Lexing

module SyntaxStyle =
    let [<Literal>] Comments = 0
    let [<Literal>] OtherTokens = 1
    let [<Literal>] Keyword = 2
    let [<Literal>] TextLiteral = 3
    let [<Literal>] NumericLiteral = 4

///Code editor services for e.g. syntax highlighting a range of text.
module CodeEditorService =
    let textColorRanges text =
        seq {
            let lexbuff = LexBuffer<_>.FromString(text)
            
            let curRange () =
                lexbuff.StartPos.AbsoluteOffset, lexbuff.EndPos.AbsoluteOffset

            while not lexbuff.IsPastEndOfStream do
                match lexbuff |> Lexer.tokenize with
                //keywords
                | Parser.token.BREAK _
                | Parser.token.CHECKED _
                | Parser.token.CONTINUE _
                | Parser.token.DEFAULT _
                | Parser.token.IF _
                | Parser.token.ELSE _
                | Parser.token.IN _
                | Parser.token.NULL _
                | Parser.token.OPEN _
                | Parser.token.TYPE _
                | Parser.token.UNCHECKED
                | Parser.token.WHILE _
                | Parser.token.THROW
                | Parser.token.TRY
                | Parser.token.CATCH
                | Parser.token.FINALLY 
                    -> yield curRange(), SyntaxStyle.Keyword
                //string and char literals
                | Parser.token.CHAR _
                | Parser.token.STRING _
                    -> yield curRange(), SyntaxStyle.TextLiteral
                //numeric literals
                | Parser.token.BOOL _
                | Parser.token.BYTE _
                | Parser.token.DOUBLE _
                | Parser.token.INT16 _
                | Parser.token.INT32 _
                | Parser.token.INT64 _
                | Parser.token.SBYTE _
                | Parser.token.SINGLE _
                | Parser.token.UINT16 _
                | Parser.token.UINT32 _
                | Parser.token.UINT64 _
                    -> yield curRange(), SyntaxStyle.NumericLiteral
                | _ 
                    -> yield curRange(), SyntaxStyle.OtherTokens
        }


