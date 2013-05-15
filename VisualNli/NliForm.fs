namespace Swensen.NL.VisualNli

open System
open System.Drawing
open System.Windows.Forms
open Swensen.FsEye.Forms

open System.Text.RegularExpressions
open Swensen.NL

open ScintillaNET

///Info about a CodeEditor file (Name=None implies new file)
type EditorFile = { Modified:bool; Name:string option }

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
    let statusLabel = new ToolStripStatusLabel(Text="Welcome to VisualNli!", Spring=true, TextAlign=ContentAlignment.MiddleRight)
    do
        statusStrip.Items.Add(statusLabel) |> ignore
        this.Controls.Add(statusStrip)

    ///Info about the file being edited in the CodeEditor  (currently we support only one open file at a time).
    let mutable editorFile = { Modified=false; Name=None }
    let updateEditorFile ef =
        if editorFile <> ef then
            editorFile <- ef
            this.Text <- 
                match editorFile with
                | {Modified=false; Name=None} -> "VisualNli"
                | {Modified=true; Name=None} -> "*VisualNli"
                | {Modified=false; Name=Some(name)} -> sprintf "%s - VisualNli" name
                | {Modified=true; Name=Some(name)} -> sprintf "*%s - VisualNli" name

    do editor.TextInsertedOrDeleted.Add(fun _ -> updateEditorFile { editorFile with Modified=true })

    let updateStatus text = 
        statusLabel.Text <- text
        statusStrip.Update()

    do
        let errorIndicator = editor.Indicators.Item(0)
        errorIndicator.Style <- IndicatorStyle.Squiggle
        errorIndicator.Color <- Color.Red

        let warningIndicator = editor.Indicators.Item(1)
        warningIndicator.Style <- IndicatorStyle.Squiggle
        warningIndicator.Color <- Color.Blue

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
        let offset = range.Start
        [0;1] |> Seq.iter (fun i -> editor.GetRange().ClearIndicator(i))
        results 
        |> Seq.choose (fun (_,value,_) -> match value with | :? CompilerMessage as value -> Some(value) | _ -> None)
        |> Seq.sortBy (fun cm -> if cm.Level = Error then 1 else 0) //errors are more important than warnings, so highlight after warnings
        |> Seq.iter (fun value ->
            let range = editor.GetRange(offset + value.Range.Start.AbsoluteOffset-1, offset + value.Range.End.AbsoluteOffset-1)
            range.SetIndicator(if value.Level = MessageLevel.Error then 0 else 1)
            
        )

        //scroll to last line in console output
        if(results.Length > 0) then
            treeView.Nodes.[treeView.Nodes.Count - 1].EnsureVisible()

        updateStatus (sprintf "Submission processed in %ims with %i warning(s) and %i error(s)" stats.Time stats.WarningCount stats.ErrorCount)

    //event handlers
    do editor.Submit.Add submit

    //main menu
    let fileDialogFilter = "NL files (*.nl)|*.nl|All files (*.*)|*.*"
    let openFileDialog = new OpenFileDialog(Filter=fileDialogFilter)
    let saveFileDialog = new SaveFileDialog(Filter=fileDialogFilter)

    let saveAs() =
        saveFileDialog.FileName <-
            match editorFile.Name with
            | Some(name) -> name | None -> ""

        let dialogResult = saveFileDialog.ShowDialog()
        if dialogResult = DialogResult.OK then
            let fileName = saveFileDialog.FileName
            System.IO.File.WriteAllText(fileName, editor.Text)
            updateEditorFile { Modified=false; Name=Some(fileName) }
            updateStatus "File saved"
        else
            updateStatus "File save cancelled"
        
        dialogResult

    let saveOrSaveAs() =
        match editorFile with
        | { Modified=true; Name=Some(name) } -> 
            System.IO.File.WriteAllText(name, editor.Text)
            updateEditorFile { editorFile with Modified=false }
            updateStatus ("File saved")
            DialogResult.None
        | { Modified=true; Name=None } -> 
            saveAs()
        | _ -> 
            updateStatus ("No file changes to save")
            DialogResult.None

    let maybePromptForSaveChanges() =
        if editorFile.Modified then
            let fileName =
                match editorFile.Name with
                | Some(name) -> name | None -> "new file"
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
                                        updateEditorFile { Modified=false; Name=None }
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
                                        editor.Text <- System.IO.File.ReadAllText(fileName)
                                        updateEditorFile { Modified=false; Name=Some(fileName) }
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