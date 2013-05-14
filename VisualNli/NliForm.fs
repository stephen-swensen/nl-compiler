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

    let submit code =
        updateStatus "Processing submission..."
        outputScintilla.Text <- ""
        outputScintilla.Update()
            
        outputScintilla.RedirectConsoleOutput <-true
        let stats, results = nli.Submit(code)
        outputScintilla.RedirectConsoleOutput <-false
            
        Control.update treeView <| fun () ->
            for name, value, ty in results do
                treeView.Watch(name, value, ty)

        if(results.Length > 0) then
            treeView.Nodes.[treeView.Nodes.Count - 1].EnsureVisible()

        updateStatus (sprintf "Submission processed in %ims with %i warning(s) and %i error(s)" stats.Time stats.WarningCount stats.ErrorCount)

    //event handlers
    do editor.Submit.Add submit

    //main menu
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
                                        updateEditorFile { Modified=false; Name=Some(dialog.FileName) }
                                        updateStatus ("Filed opened: " + dialog.FileName)
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