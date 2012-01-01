namespace VisualNli

open System
open System.Drawing
open System.Windows.Forms
open Swensen.FsEye.Forms

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
    let mutable rtb:RichTextBox = null
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
                    rtb <- new RichTextBox(Dock=DockStyle.Fill, Font=textFont)
                    splitc.Panel1.Controls.Add(rtb)
                do
                    treeViewPanel <- new Panel(Dock=DockStyle.Fill, BackColor=System.Drawing.SystemColors.Control)
                    do
                        treeView <- new WatchTreeView(Dock=DockStyle.Fill, Font=textFont)
                        treeViewPanel.Controls.Add(treeView)
                    splitc.Panel2.Controls.Add(treeViewPanel)
                this.Controls.Add(splitc)

            
            do //setup ALT+ENTER event on rich text box
                rtb.KeyDown.Add <| fun args ->
                    if args.Alt && args.KeyCode = Keys.Enter && not <| String.IsNullOrWhiteSpace(rtb.SelectedText) then
                        try
                            match nli.TrySubmit(rtb.SelectedText) with
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