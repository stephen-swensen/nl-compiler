namespace Swensen.NL.VisualNli

open System
open System.Drawing
open System.Windows.Forms
open Swensen.FsEye.Forms

open System.Text.RegularExpressions
open Swensen.NL
open System.IO

open ScintillaNET

type CallTipInfo = { Messages:CompilerMessage[]; Offset:int }

///Info about a CodeEditor file (Name=None implies new file)
type EditorFile = { Modified:bool; Info:FileInfo option }

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
    let editorFont = new Font("Consolas", 10.0f)

    //controls
    let splitc = new System.Windows.Forms.SplitContainer(Dock=DockStyle.Fill, Orientation=Orientation.Horizontal, BackColor=Color.LightGray)
    
    let editor = new CodeEditor(editorFont, Dock=DockStyle.Fill)
    do splitc.Panel1.Controls.Add(editor)

    let tabControl = new TabControl(Dock=DockStyle.Fill)
    
    let watchTab = new TabPage("Watch")
    let treeViewPanel = new Panel(Dock=DockStyle.Fill, BackColor=System.Drawing.SystemColors.Control)
    let treeView = new WatchTreeView(Dock=DockStyle.Fill)
    do
        treeViewPanel.Controls.Add(treeView)
        watchTab.Controls.Add(treeViewPanel)
        tabControl.TabPages.Add(watchTab)

    let outputTab = new TabPage("Output")
    let outputScintilla = new OutputScintilla(editorFont, Dock=DockStyle.Fill)
    do 
        outputTab.Controls.Add(outputScintilla)
        tabControl.TabPages.Add(outputTab)

    do
        splitc.Panel2.Controls.Add(tabControl)
        this.Controls.Add(splitc)
    
    let statusStrip = new StatusStrip(Dock=DockStyle.Bottom)
    let statusLabel = new ToolStripStatusLabel(Text="Welcome to VisualNli!", Spring=true, TextAlign=ContentAlignment.MiddleLeft)
    let statusPositionLabel = new ToolStripStatusLabel(Text="Pos: 1,1", Spring=true, TextAlign=ContentAlignment.MiddleRight)

    do
        statusStrip.Items.Add(statusLabel) |> ignore
        statusStrip.Items.Add(statusPositionLabel) |> ignore
        this.Controls.Add(statusStrip)
        
        editor.CaretChanged.Add(fun e -> statusPositionLabel.Text <- sprintf "Pos: %i,%i" (e.Line+1) (e.Column+1))

    ///Info about the file being edited in the CodeEditor  (currently we support only one open file at a time).
    let mutable editorFile = { Modified=false; Info=None }
    let updateEditorFile ef =
        if editorFile <> ef then
            editorFile <- ef
            this.Text <- 
                match editorFile with
                | {Modified=false; Info=None} -> "VisualNli"
                | {Modified=true; Info=None} -> "*VisualNli"
                | {Modified=false; Info=Some(info)} -> sprintf "%s (%s) - VisualNli" info.Name info.FullName
                | {Modified=true; Info=Some(info)} -> sprintf "*%s (%s) - VisualNli" info.Name info.FullName

    do editor.TextInsertedOrDeleted.Add(fun _ -> updateEditorFile { editorFile with Modified=true })

    let updateStatus text = 
        statusLabel.Text <- text
        statusStrip.Update()

    //Be careful to only set this on the gui thread to avoid race conditions
    let mutable callTipInfo = {Messages=[||]; Offset=0}

    let submit (range:Range) =
        updateStatus "Processing submission..."
        outputScintilla.Text <- ""
        outputScintilla.Update()
            
        //submit results with console output (stdout and stderr, including errors and warnings) redirected to console tab
        outputScintilla.RedirectConsoleOutput <-true
        let code = range.Text
        let stats, results = nli.Submit(code)
        outputScintilla.RedirectConsoleOutput <-false
            
        //update watches
        Control.update treeView <| fun () ->
            for name, value, ty in results do
                treeView.Watch(name, value, ty)

        //draw squiggly indicators for errors and warnings
        let messages = 
            results 
            |> Array.choose (fun (_,value,_) -> match value with | :? CompilerMessage as value -> Some(value) | _ -> None)

        let offset = range.Start
        callTipInfo <- {Messages=messages; Offset=offset}
        editor.ResetIndicators(offset, messages)

        //scroll to last line in console output
        if(results.Length > 0) then
            treeView.Nodes.[treeView.Nodes.Count - 1].EnsureVisible()

        updateStatus (sprintf "Submission processed in %ims with %i warning(s) and %i error(s)" stats.Time stats.WarningCount stats.ErrorCount)

    //real-time error indicators
    do
        editor.TextInsertedOrDeleted.Add <| fun _ ->
            Async.CancelDefaultToken()
            let guiContext = System.Threading.SynchronizationContext.Current
            async {
                let backgroundContext = System.Threading.SynchronizationContext.Current //always null - don't understand the point
                do! Async.Sleep(300)
                do! Async.SwitchToContext guiContext
                let code = editor.Text
                do! Async.SwitchToContext backgroundContext
                EL.InstallInMemoryLogger()
                Compilation.lexParseAndSemant code |> ignore
                let messages = EL.ActiveLogger.GetMessages()
                do! Async.SwitchToContext guiContext
                callTipInfo <- {Messages=messages; Offset=0}
                editor.ResetIndicators  (0, messages)
                do! Async.SwitchToContext backgroundContext
            } |> Async.Start

    do
        editor.NativeInterface.SetMouseDwellTime(400)
        editor.DwellStart.Add <| fun e -> 
            let dwellMessages = 
                callTipInfo.Messages
                |> Array.filter (fun msg -> (msg.Range.Start.AbsoluteOffset+callTipInfo.Offset) <= e.Position && (msg.Range.End.AbsoluteOffset+callTipInfo.Offset) >= e.Position)

            if dwellMessages |> Array.isEmpty |> not then
                let tip = 
                    dwellMessages 
                    |> Seq.map (fun msg -> sprintf "%s: %s" msg.CodeName msg.Message)
                    |> String.concat "\n"
                editor.CallTip.Show(tip, e.Position)
        
        editor.DwellEnd.Add <| fun e -> editor.CallTip.Hide()

    //event handlers
    do editor.Submit.Add submit

    //main menu
    let fileDialogFilter = "NL files (*.nl)|*.nl|All files (*.*)|*.*"
    let openFileDialog = new OpenFileDialog(Filter=fileDialogFilter)
    let saveFileDialog = new SaveFileDialog(Filter=fileDialogFilter)

    let saveAs() =
        saveFileDialog.FileName <-
            match editorFile.Info with
            | Some(fi) -> fi.Name | None -> ""

        let dialogResult = saveFileDialog.ShowDialog()
        if dialogResult = DialogResult.OK then
            let fileName = saveFileDialog.FileName
            File.WriteAllText(fileName, editor.Text)
            updateEditorFile { Modified=false; Info=Some(FileInfo(fileName)) }
            updateStatus "File saved"
        else
            updateStatus "File save cancelled"
        
        dialogResult

    let saveOrSaveAs() =
        match editorFile with
        | { Modified=true; Info=Some(fi) } -> 
            File.WriteAllText(fi.FullName, editor.Text)
            updateEditorFile { editorFile with Modified=false }
            updateStatus ("File saved")
            DialogResult.None
        | { Modified=true; Info=None } -> 
            saveAs()
        | _ -> 
            updateStatus ("No file changes to save")
            DialogResult.None

    let maybePromptForSaveChanges() =
        if editorFile.Modified then
            let fileName =
                match editorFile.Info with
                | Some(fi) -> fi.Name | None -> "new file"
            MessageBox.Show(sprintf "Save changes to %s?" fileName, "Save Changes", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)    
        else
            DialogResult.No

    do
        this.Menu <- 
            new MainMenu(
                [|
                    yield (
                        let fileMi = new MenuItem("File") //shortcut
                        fileMi.MenuItems.AddRange [|
                            yield (
                                let mi = new MenuItem("New")
                                mi.Shortcut <- Shortcut.CtrlN
                                mi.Click.Add <| fun _ ->
                                    let mutable promptResult = maybePromptForSaveChanges()
                                    if promptResult = DialogResult.Yes then
                                        promptResult <- saveOrSaveAs()
                                    
                                    if promptResult <> DialogResult.Cancel then
                                        editor.Text <- ""
                                        updateEditorFile { Modified=false; Info=None }
                                        updateStatus ("New file opened")
                                    else
                                        updateStatus ("New file open cancelled")
                                mi
                            )
                            yield (
                                let mi = new MenuItem("Open...")
                                mi.Shortcut <- Shortcut.CtrlO
                                mi.Click.Add <| fun _ ->
                                    let mutable promptResult = maybePromptForSaveChanges()
                                    if promptResult = DialogResult.Yes then
                                        promptResult <- saveOrSaveAs()
                                    
                                    openFileDialog.FileName <- ""
                                    if promptResult <> DialogResult.Cancel && openFileDialog.ShowDialog() = DialogResult.OK then
                                        let fileName = openFileDialog.FileName
                                        editor.Text <- File.ReadAllText(fileName)
                                        updateEditorFile { Modified=false; Info=Some(FileInfo(fileName)) }
                                        updateStatus ("File opened")
                                    else
                                        updateStatus ("File open cancelled")
                                mi
                            )
                            yield (
                                let mi = new MenuItem("Save")
                                mi.Shortcut <- Shortcut.CtrlS
                                mi.Click.Add <| fun _ ->
                                    saveOrSaveAs() |> ignore
                                mi
                            )
                            yield (
                                let mi = new MenuItem("Save As...")
                                mi.Click.Add <| fun _ ->
                                    saveAs() |> ignore
                                mi
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
                                let submitMi = new MenuItem("Submit")
                                submitMi.Click.Add (fun _ -> editor.TriggerSubmit())
                                submitMi
                            )
                            yield (
                                let resetMi = new MenuItem("Reset")
                                resetMi.Click.Add <| fun _ ->
                                    nli.Reset()
                                    treeView.ClearAll()
                                    outputScintilla.Text <- ""
                                    updateStatus "Session reset"
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
                                mi.Click.Add(fun _ -> treeView.Archive(); updateStatus "Watches archived")
                                mi
                            )
                            yield new MenuItem("-")
                            yield (
                                let mi = new MenuItem("Clear Archives")
                                mi.Click.Add(fun _ -> treeView.ClearArchives(); updateStatus "Archives cleared")
                                mi
                            )
                            yield (
                                let mi = new MenuItem("Clear Watches")
                                mi.Click.Add(fun _ -> treeView.ClearWatches(); updateStatus "Watches cleared")
                                mi
                            )
                            yield (
                                let mi = new MenuItem("Clear All")
                                mi.Click.Add(fun _ -> treeView.ClearAll(); updateStatus "All cleared")
                                mi
                            )
                        |]
                        watchesMi
                    )
                |]
            )
    do
        this.FormClosing.Add(fun e ->
            let mutable promptResult = maybePromptForSaveChanges()
            if promptResult = DialogResult.Yes then
                promptResult <- saveOrSaveAs()

            if promptResult = DialogResult.Cancel then
                e.Cancel <- true
                updateStatus "Exit cancelled"
        )