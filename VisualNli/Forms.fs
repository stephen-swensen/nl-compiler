namespace Swensen.NL.VisualNli

open System
open System.Drawing
open System.Windows.Forms
open Swensen.FsEye.Forms

open System.Runtime.InteropServices

open System.Text.RegularExpressions
open System.Drawing
open Swensen.NL
open Microsoft.FSharp.Text.Lexing

open ScintillaNET

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
                    -> yield curRange(), 1
                //string and char literals
                | Parser.token.CHAR _
                | Parser.token.STRING _
                    -> yield curRange(), 2
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
                    -> yield curRange(), 3
                | _ 
                    -> yield curRange(), 0 //default
        }

[<AllowNullLiteral>]
type CodeEditor(font:Font) as this =
    inherit StandardScintilla()
    
    let submitEvent = new Event<_>()

    do
        //http://scintillanet.codeplex.com/wikipage?title=FAQ
        this.Indentation.SmartIndentType <- SmartIndent.None;
        this.ConfigurationManager.Language <- String.Empty;
        this.Lexing.LexerName <- "container";
        this.Lexing.Lexer <- Lexer.Container;

        let defaultStyle = this.Styles.[0] in
            defaultStyle.Font <- font

        let keywordStyle = this.Styles.[1] in
            keywordStyle.ForeColor <- Color.Blue;
            keywordStyle.Font <- font

        let textStyle = this.Styles.[2] in
            textStyle.ForeColor <- Color.DarkRed;
            textStyle.Font <- font

        let numberStyle = this.Styles.[3] in
            numberStyle.ForeColor <- Color.Teal;
            numberStyle.Font <- font

        this.StyleNeeded.Add (fun e ->
            let text = e.Range.Text
            let offset = e.Range.Start
            CodeEditorService.textColorRanges text
            |> Seq.iter(fun ((startPos, endPos), styleIndex) ->
                let startPos, endPos = startPos+offset, endPos+offset
                let range = this.GetRange(startPos,endPos)
                range.SetStyle(styleIndex) |> ignore))

    member this.Submit = submitEvent.Publish

    ///Alt+Enter submits selection if any, otherwise all text
    override this.OnKeyDown(e:KeyEventArgs) =
        base.OnKeyDown(e)
        if e.Alt && e.KeyCode = Keys.Enter then
            if this.Selection.Length > 0 then
                submitEvent.Trigger(this.Selection.Text)
            else
                submitEvent.Trigger(this.Text)

            e.SuppressKeyPress <- true //so doesn't make "ping" noise
        else 
            ()

type NliSessionManager() =
    //data
    let mutable nli = Swensen.NL.Nli()
    let mutable errorCount = 0
    let mutable warningCount = 0
    let mutable exnCount = 0

    member __.Reset() =
        nli <- Swensen.NL.Nli()
        errorCount <- 0
        warningCount <- 0
        exnCount <- 0


    member __.Submit(code:String) = 
        let collectMessages() = [|  
            let msgs = Swensen.NL.MessageLogger.ActiveLogger.GetMessages()
            for msg in msgs do
                match msg.Level with
                | MessageLevel.Error ->
                    yield (sprintf "error%i" errorCount,  msg :> obj, msg.GetType())
                    errorCount <- errorCount + 1 
                | MessageLevel.Warning ->
                    yield (sprintf "warning%i" warningCount,  msg :> obj, msg.GetType())
                    warningCount <- warningCount + 1
        |]

        try
            match nli.TrySubmit(code) with
            | Some(results) -> [| yield! collectMessages(); yield! results |]
            | None -> collectMessages()
        with e ->
            let result = sprintf "exn%i" exnCount, e :> obj, e.GetType()
            exnCount <- exnCount + 1
            [| yield! collectMessages(); yield result |]


type public NliForm() as this =
    inherit Form(
        Icon = null,
        Text = "VisualNli", 
        Size = (
            let size = SystemInformation.PrimaryMonitorSize
            System.Drawing.Size((3 * size.Width) / 4, (2 * size.Height) / 3)
        )
    )

    let nli = NliSessionManager()
    //data
    let textFont = 
        let fontFamily = FontFamily.GenericMonospace //i.e. Courier New on Windows
        new Font(fontFamily, 11.0f)

    //controls
    let splitc = new System.Windows.Forms.SplitContainer(Dock=DockStyle.Fill, Orientation=Orientation.Horizontal, BackColor=Color.LightGray)
    
    let editor = new CodeEditor(textFont, Dock=DockStyle.Fill)
    do splitc.Panel1.Controls.Add(editor)

    let treeViewPanel = new Panel(Dock=DockStyle.Fill, BackColor=System.Drawing.SystemColors.Control)
    let treeView = new WatchTreeView(Dock=DockStyle.Fill, Font=textFont)
    do treeViewPanel.Controls.Add(treeView)
    do splitc.Panel2.Controls.Add(treeViewPanel)

    do this.Controls.Add(splitc)

    do
        this.Menu <- 
            new MainMenu(
                [|
                    yield (
                        let fileMi = new MenuItem("File")
                        fileMi.MenuItems.AddRange [|
                            yield (
                                let exitMi = new MenuItem("Open")
                                exitMi.Click.Add <| fun _ ->
                                    let dialog = new OpenFileDialog()
                                    if dialog.ShowDialog() = DialogResult.OK then
                                        editor.Text <- System.IO.File.ReadAllText(dialog.FileName)
                                exitMi
                            )

                            yield new MenuItem("-")
                        
                            yield (
                                let exitMi = new MenuItem("Exit")
                                exitMi.Click.Add(fun _ -> Application.Exit())
                                exitMi
                            )
                        |]
                        fileMi
                    )
                    yield (
                        let sessionMi = new MenuItem("Session")
                        sessionMi.MenuItems.AddRange [|
                            yield (
                                let resetMi = new MenuItem("Reset")
                                resetMi.Click.Add <| fun _ ->
                                    nli.Reset()
                                    treeView.ClearAll()
                                resetMi
                            )
                        |]
                        sessionMi
                    )
                    yield (
                        let watchesMi = new MenuItem("Watches")
                        watchesMi.MenuItems.AddRange [|
                            yield (
                                let mi = new MenuItem("Archive Watches")
                                mi.Click.Add(fun _ -> treeView.Archive())
                                mi
                            )
                            yield new MenuItem("-")
                            yield (
                                let mi = new MenuItem("Clear Archives")
                                mi.Click.Add(fun _ -> treeView.ClearArchives())
                                mi
                            )
                            yield (
                                let mi = new MenuItem("Clear Watches")
                                mi.Click.Add(fun _ -> treeView.ClearWatches())
                                mi
                            )
                            yield (
                                let mi = new MenuItem("Clear All")
                                mi.Click.Add(fun _ -> treeView.ClearAll())
                                mi
                            )
                        |]
                        watchesMi
                    )
                |]
            )

    //event handlers
    do 
        editor.Submit.Add <| fun code ->
            treeView.BeginUpdate()
            for name, value, ty in nli.Submit(code) do
                treeView.Watch(name, value, ty)
                //add in reverse order (should have this functionality part of the watch tree view itself)
                let lastAdded = treeView.Nodes.[treeView.Nodes.Count - 1]
                treeView.Nodes.RemoveAt(treeView.Nodes.Count - 1)
                treeView.Nodes.Insert(0, lastAdded)
            treeView.EndUpdate()