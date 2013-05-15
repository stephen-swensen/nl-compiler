namespace Swensen.NL.VisualNli
open System
open System.Drawing
open System.Windows.Forms

open ScintillaNET

[<AllowNullLiteral>]
type CodeEditor(font:Font) as this =
    inherit StandardScintilla()
    
    let submitEvent = new Event<_>()

    do
        this.Margins.[0].Width <- 22;

        //http://scintillanet.codeplex.com/wikipage?title=FAQ
        this.Indentation.BackspaceUnindents <- true;
        this.Indentation.SmartIndentType <- SmartIndent.Simple;
        this.ConfigurationManager.Language <- String.Empty;
        this.Lexing.LexerName <- "container";
        this.Lexing.Lexer <- Lexer.Container;

        let stylesConfig = [
            (0, Color.Black)
            (1, Color.Blue)
            (2, Color.DarkRed)
            (3, Color.Teal)
        ]

        for (index, color) in stylesConfig do
            let style = this.Styles.[index]
            style.ForeColor <- color
            style.Font <- font

        this.StyleNeeded.Add (fun e ->
            let text = e.Range.Text
            let offset = e.Range.Start
            CodeEditorService.textColorRanges text
            |> Seq.iter(fun ((startPos, endPos), styleIndex) ->
                let startPos, endPos = startPos+offset, endPos+offset
                let range = this.GetRange(startPos,endPos)
                range.SetStyle(styleIndex) |> ignore))

    member this.Submit = submitEvent.Publish

    ///Triggers the Submit event with the selected text of all the text if none selected
    member this.TriggerSubmit() =
        if this.Selection.Length > 0 then
            submitEvent.Trigger(this.Selection.Range)
        else
            submitEvent.Trigger(this.GetRange())

    ///Alt+Enter submits selection if any, otherwise all text
    override this.OnKeyDown(e:KeyEventArgs) =
        base.OnKeyDown(e)
        if e.Alt && e.KeyCode = Keys.Enter then
            this.TriggerSubmit()
            e.SuppressKeyPress <- true //so doesn't make "ping" noise
        else 
            ()