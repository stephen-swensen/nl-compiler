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

    let nli = Swensen.NL.Nli()

    let render f =
        this.SuspendLayout();
        f()
        this.ResumeLayout(false)
        this.PerformLayout()

    do
        render <| fun () ->
            do //build the visual tree
                let splitc = new System.Windows.Forms.SplitContainer(Dock=DockStyle.Fill, Orientation=Orientation.Horizontal, BackColor=Color.LightGray)

                let rtb = new RichTextBox(Dock=DockStyle.Fill, Font=new Font(FontFamily.GenericMonospace, 14.0f))
                splitc.Panel1.Controls.Add(rtb)

                let watchp = new WatchPanel(Dock=DockStyle.Fill, BackColor=System.Drawing.SystemColors.Control)
                splitc.Panel2.Controls.Add(watchp)

                this.Controls.Add(splitc)

                let errorsCount = ref 0
                let exnCount = ref 0
                rtb.KeyDown.AddHandler(new KeyEventHandler(fun sender args ->
                    if args.Alt && args.KeyCode = Keys.Enter && not <| String.IsNullOrWhiteSpace(rtb.SelectedText) then
                        try
                            match nli.TrySubmit(rtb.SelectedText) with
                            | Some(results) ->
                                for name, value, ty in results do
                                    watchp.Watch(name, value, ty)
                            | None ->
                                watchp.Watch(sprintf "errors%i" !errorsCount, Swensen.NL.ErrorLogger.ActiveLogger.GetErrors())
                                errorsCount := !errorsCount + 1                             
                        with e ->
                            watchp.Watch(sprintf "exn%i" !exnCount, e)
                            exnCount := !exnCount + 1
                    else 
                        ()
                ))