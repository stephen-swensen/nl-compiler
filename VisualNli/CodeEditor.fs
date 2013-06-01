namespace Swensen.NL.VisualNli
open System
open System.Drawing
open System.Windows.Forms
open Swensen.NL

open ScintillaNET

[<AllowNullLiteral>]
type CodeEditor(font:Font) as this =
    inherit StandardScintilla()
    
    let submitEvent = new Event<_>()

    do
        this.Margins.[0].Width <- 44;

        let errorIndicator = this.Indicators.Item(0)
        errorIndicator.Style <- IndicatorStyle.Squiggle
        errorIndicator.Color <- Color.Red

        let warningIndicator = this.Indicators.Item(1)
        warningIndicator.Style <- IndicatorStyle.Squiggle
        warningIndicator.Color <- Color.Blue

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

    ///Ctrl+Enter submits selection if any, otherwise all text
    override this.OnKeyDown(e:KeyEventArgs) =
        base.OnKeyDown(e)
        if e.Control && e.KeyCode = Keys.Enter then
            this.TriggerSubmit()
            e.SuppressKeyPress <- true //so doesn't make "ping" noise
        else 
            ()

    ///Clears all indicators
    member this.ClearIndicators() =
        [0;1] |> Seq.iter (fun i -> this.GetRange().ClearIndicator(i))

    ///Clear all indicators (for the entire document) and then draw indicators for the given compiler messages with the given position offset
    member this.ResetIndicators(compilerMessages:CompilerMessage seq) =
        this.ClearIndicators()
        compilerMessages
        |> Seq.iter (fun value ->
            let range = this.GetRange(value.Range.Start.AbsoluteOffset, value.Range.End.AbsoluteOffset)
            range.SetIndicator(if value.Level = MessageLevel.Error then 0 else 1)
        )