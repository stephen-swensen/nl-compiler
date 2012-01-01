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

[<AllowNullLiteral>]
type CodeEditor() =
    inherit RichTextBox()

    override X.OnTextChanged(e : System.EventArgs) =
        base.OnTextChanged(e)
        X.Colorize()

    ///originally based on http://fssnip.net/5L
    member this.Colorize() =
        let curSelectionStart = this.SelectionStart
        Win32.LockWindowUpdate(this.Handle.ToInt32())
        
        do
            let lexbuff = LexBuffer<_>.FromString(this.Text)
            
            let colorCurToken color =
                this.SelectionStart    <- lexbuff.StartPos.AbsoluteOffset
                this.SelectionLength   <- lexbuff.EndPos.AbsoluteOffset - lexbuff.StartPos.AbsoluteOffset
                this.SelectionColor    <- color

            while not lexbuff.IsPastEndOfStream do
                match lexbuff |> Lexer.tokenize with
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
                    -> colorCurToken Color.Blue
//                | Parser.token.BOOL _
//                | Parser.token.BYTE _
                | Parser.token.CHAR _
//                | Parser.token.DOUBLE _
//                | Parser.token.INT16 _
//                | Parser.token.INT32 _
//                | Parser.token.INT64 _
//                | Parser.token.SBYTE _
//                | Parser.token.SINGLE _
                | Parser.token.STRING _
//                | Parser.token.UINT16 _
//                | Parser.token.UINT32 _
//                | Parser.token.UINT64 _
                    -> colorCurToken Color.DarkRed
                | _ -> ()
                
        this.SelectionStart    <- curSelectionStart
        this.SelectionLength   <- 0
        this.SelectionColor    <- Color.Black

        Win32.LockWindowUpdate(0)


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