namespace VisualNli

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
    [<DllImport(@"User32", CharSet = CharSet.Ansi, SetLastError = false, ExactSpelling = true)>]
    extern void LockWindowUpdate(int hWnd)

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
                | _ -> yield curRange(), Color.Black
        }

[<AllowNullLiteral>]
type CodeEditor() as this =
    inherit RichTextBox(AcceptsTab=true)

    ///strategy based on http://fssnip.net/5L
    let colorize offset text =
        let curSelectionStart = this.SelectionStart
        Win32.LockWindowUpdate(this.Handle.ToInt32())

        CodeEditorService.textColorRanges text
        |> Seq.iter(fun ((startPos, endPos), color) ->
            this.SelectionStart     <- offset + startPos
            this.SelectionLength    <- endPos - startPos
            this.SelectionColor     <- color)
                
        this.SelectionStart    <- curSelectionStart
        this.SelectionLength   <- 0
        this.SelectionColor    <- Color.Black

        Win32.LockWindowUpdate(0)

    override this.OnKeyDown(e:KeyEventArgs) =
        base.OnKeyDown(e)

    override this.OnTextChanged(e:System.EventArgs) =
        base.OnTextChanged(e)
        let offset = this.GetFirstCharIndexOfCurrentLine()
        let curLine = this.Lines.[this.GetLineFromCharIndex(offset)]
        colorize offset curLine


type public MainForm() as this =
    inherit Form(
        Icon = null,
        Text = "VisualNli", 
        Size = (
            let size = SystemInformation.PrimaryMonitorSize
            System.Drawing.Size((2 * size.Width) / 3, size.Height / 2)
        )
    )

    //control declarations
    let nli = Swensen.NL.Nli()
    let mutable splitc:SplitContainer = null
    let mutable editor:CodeEditor = null
    let mutable treeViewPanel = null
    let mutable treeView:WatchTreeView = Unchecked.defaultof<WatchTreeView>

    //other declarations
    let mutable textFont = new Font(FontFamily.GenericMonospace, 13.0f)
    let errorsCount = ref 0
    let exnCount = ref 0

    let render f =
        this.SuspendLayout();
        f()
        this.ResumeLayout(false)
        this.PerformLayout()

    do
        render <| fun () ->
            do //build the visual tree
                splitc <- new System.Windows.Forms.SplitContainer(Dock=DockStyle.Fill, Orientation=Orientation.Horizontal, BackColor=Color.LightGray)
                do
                    editor <- new CodeEditor(Dock=DockStyle.Fill, Font=textFont)
                    splitc.Panel1.Controls.Add(editor)
                do
                    treeViewPanel <- new Panel(Dock=DockStyle.Fill, BackColor=System.Drawing.SystemColors.Control)
                    do
                        treeView <- new WatchTreeView(Dock=DockStyle.Fill, Font=textFont)
                        treeViewPanel.Controls.Add(treeView)
                    splitc.Panel2.Controls.Add(treeViewPanel)
                this.Controls.Add(splitc)

            do //setup ALT+ENTER event on rich text box 
                editor.KeyDown.Add <| fun args ->
                    if args.Alt && args.KeyCode = Keys.Enter && not <| String.IsNullOrWhiteSpace(editor.SelectedText) then
                        try
                            match nli.TrySubmit(editor.SelectedText) with
                            | Some(results) ->
                                for name, value, ty in results do
                                    treeView.Watch(name, value, ty)
                            | None ->
                                treeView.Watch(sprintf "errors%i" !errorsCount, Swensen.NL.ErrorLogger.ActiveLogger.GetErrors())
                                errorsCount := !errorsCount + 1                             
                        with e ->
                            treeView.Watch(sprintf "exn%i" !exnCount, e)
                            exnCount := !exnCount + 1

                        args.SuppressKeyPress <- true //so doesn't make "ping" noise
                    else 
                        ()