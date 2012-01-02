﻿namespace VisualNli

open System
open System.Drawing
open System.Windows.Forms
open Swensen.FsEye.Forms

open System.Runtime.InteropServices

open System.Text.RegularExpressions
open System.Drawing
open Swensen.NL
open Microsoft.FSharp.Text.Lexing

module Win32 =
    module DllImports =
        [<DllImport(@"User32", CharSet = CharSet.Ansi, SetLastError = false, ExactSpelling = true)>]
        extern void LockWindowUpdate(nativeint hWnd)

    open DllImports

    //make as extension method to Control?
    let LockWindowUpdate (cntrl:Control) f = 
        try
            LockWindowUpdate(cntrl.Handle) //not really supposed to use lockwindowupdate for non drag/drop scenarios...
            f()
        finally
            LockWindowUpdate(0n)

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
                | Parser.token.ELSE _
                | Parser.token.IN _
                | Parser.token.NULL _
                | Parser.token.OPEN _
                | Parser.token.THEN _
                | Parser.token.TYPE _
                | Parser.token.UNCHECKED
                | Parser.token.WHILE _
                    -> yield curRange(), Color.Blue
                //string and char literals
                | Parser.token.CHAR _
                | Parser.token.STRING _
                    -> yield curRange(), Color.DarkRed
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
                    -> yield curRange(), Color.Teal
                | _ ->  ()
        }

[<AllowNullLiteral>]
type CodeEditor() as self =
    inherit RichTextBox(AcceptsTab=true)

    let submitEvent = new Event<_>()
    let update = Win32.LockWindowUpdate self

    ///used to detect whether text changed is a paste event or not, see http://stackoverflow.com/a/6638841/236255
    let mutable lastCursorPos = 0

    ///offset is corresponds to the position at the start of the text being colorized.
    ///The curors will be placed at the end of text with respect to the offset
    let colorize offset (text:String) =
        CodeEditorService.textColorRanges text
        |> Seq.iter(fun ((startPos, endPos), color) ->
            self.SelectionStart     <- offset + startPos
            self.SelectionLength    <- endPos - startPos
            self.SelectionFont      <- self.Font
            self.SelectionColor     <- color)

    let resetCursor pos =
        self.SelectionStart     <- pos
        self.SelectionLength    <- 0
        self.SelectionBackColor <- self.BackColor
        self.SelectionColor     <- self.ForeColor
        self.SelectionFont      <- self.Font

    let resetSelection startPos length =

        //select what was just pasted (need to do again after last operation)
        self.SelectionStart     <- startPos
        self.SelectionLength    <- length

        //make pasted text plain styles
        self.SelectionFont      <- self.Font
        self.SelectionBackColor <- self.BackColor
        self.SelectionColor     <- self.ForeColor

    member self.Submit = submitEvent.Publish

    override self.OnKeyDown(e:KeyEventArgs) =
        base.OnKeyDown(e)
        if not (e.Control && e.KeyCode = Keys.V) then
            lastCursorPos <- self.SelectionStart
        
        
        if e.Alt && e.KeyCode = Keys.Enter && not <| String.IsNullOrWhiteSpace(self.SelectedText) then
            submitEvent.Trigger(self.SelectedText)
            e.SuppressKeyPress <- true //so doesn't make "ping" noise

    override self.OnGotFocus(e:_) =
        base.OnGotFocus(e)
        lastCursorPos <- self.SelectionStart

    override self.OnTextChanged(e:System.EventArgs) =
        base.OnTextChanged(e)
        update <| fun () ->
            if self.SelectionStart - lastCursorPos > 1 then //hack to detect pasted range of text
                //calculate pasted text start pos and length
                let cursorPos = self.SelectionStart
                let pastedLength = cursorPos - lastCursorPos
                
                //select what was just pasted
                self.SelectionStart     <- lastCursorPos
                self.SelectionLength    <- pastedLength
                let text = self.SelectedText

                //make rtf plain text
                self.SelectedText <- text

                resetSelection lastCursorPos text.Length

                //colorize it
                colorize lastCursorPos text
                resetCursor cursorPos
            else //single char text change (e.g. normal typing), for performance reasons only colorize the current line
                let cursorPos = self.SelectionStart
                let offset = max 0 (cursorPos - 500)
                let length = min (self.Text.Length - offset) (offset + 1000)

                //select text from startOfLastColorizedToken to current position
                self.SelectionStart     <- offset
                self.SelectionLength    <- length
                let text = self.SelectedText
                
                resetSelection offset text.Length
                
                colorize offset text
                resetCursor cursorPos

            lastCursorPos <- self.SelectionStart

type public NliForm() as self =
    inherit Form(
        Icon = null,
        Text = "VisualNli", 
        Size = (
            let size = SystemInformation.PrimaryMonitorSize
            System.Drawing.Size((3 * size.Width) / 4, (2 * size.Height) / 3)
        )
    )

    //data
    let nli = Swensen.NL.Nli()
    let textFont = new Font(FontFamily.GenericMonospace, 12.0f)
    let errorsCount = ref 0
    let exnCount = ref 0
    
    //controls
    let splitc = new System.Windows.Forms.SplitContainer(Dock=DockStyle.Fill, Orientation=Orientation.Horizontal, BackColor=Color.LightGray)
    
    let editor = new CodeEditor(Dock=DockStyle.Fill, Font=textFont)
    do splitc.Panel1.Controls.Add(editor)

    let treeViewPanel = new Panel(Dock=DockStyle.Fill, BackColor=System.Drawing.SystemColors.Control)
    let treeView = new WatchTreeView(Dock=DockStyle.Fill, Font=textFont)
    do treeViewPanel.Controls.Add(treeView)
    do splitc.Panel2.Controls.Add(treeViewPanel)

    do self.Controls.Add(splitc)

    //event handlers
    do 
        editor.Submit.Add <| fun code ->
            try
                match nli.TrySubmit(code) with
                | Some(results) ->
                    for name, value, ty in results do
                        treeView.Watch(name, value, ty)
                | None ->
                    treeView.Watch(sprintf "errors%i" !errorsCount, Swensen.NL.ErrorLogger.ActiveLogger.GetErrors())
                    errorsCount := !errorsCount + 1                             
            with e ->
                treeView.Watch(sprintf "exn%i" !exnCount, e)
                exnCount := !exnCount + 1