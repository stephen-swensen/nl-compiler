namespace Swensen.NL.VisualNli

open System
open System.Drawing
open System.Windows.Forms
open Swensen.FsEye.Forms

open System.Text.RegularExpressions
open Swensen.NL

open ScintillaNET

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
    let statusStrip = new StatusStrip(Dock=DockStyle.Bottom)
    let statusLabel = new ToolStripStatusLabel(Text="Welcome to VisualNli!", Spring=true, TextAlign=ContentAlignment.MiddleRight)
    do 
        statusStrip.Items.Add(statusLabel) |> ignore
        this.Controls.Add(statusStrip)

    let updateStatus text = statusLabel.Text <- text

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
                                let resetMi = new MenuItem("Reset")
                                resetMi.Click.Add <| fun _ ->
                                    nli.Reset()
                                    treeView.ClearAll()
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

    //event handlers
    do 
        editor.Submit.Add <| fun code ->
            let sw = System.Diagnostics.Stopwatch.StartNew()
            updateStatus "Processing submission..."
            statusStrip.Update()
            treeView.BeginUpdate()
            for name, value, ty in nli.Submit(code) do
                treeView.Watch(name, value, ty)
                //add in reverse order (should have this functionality part of the watch tree view itself)
                let lastAdded = treeView.Nodes.[treeView.Nodes.Count - 1]
                treeView.Nodes.RemoveAt(treeView.Nodes.Count - 1)
                treeView.Nodes.Insert(0, lastAdded)
            treeView.EndUpdate()
            sw.Stop()
            updateStatus (sprintf "Submission processed (in %ims)" sw.ElapsedMilliseconds)