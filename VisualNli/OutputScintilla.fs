﻿namespace Swensen.NL.VisualNli
open System
open System.Drawing
open System.Windows.Forms

open ScintillaNET

///A TextWritter sufficient for redirecting stdout and stderr to a Scintilla control
///including output following (scrolling control to the end of the document) 
///and updating on signficant content change.
type ScintillaTextWriter(scintilla:StandardScintilla, style:int, encoding) =
    inherit System.IO.TextWriter()

    override this.Write(c:char) =
        if not this.Enabled then ()
        else
            base.Write(c)
            let writeOutputWin = fun () ->
                scintilla.SuspendReadonly(fun () -> 
                    let range = scintilla.AppendText(c |> string)
                    range.SetStyle(style)
            
                    if c = '\n' then 
                        scintilla.Scrolling.ScrollBy(0, scintilla.Lines.Count)
                        scintilla.Update())
            
            if not scintilla.InvokeRequired then 
                writeOutputWin()

    //no point in overriding WriteLine since only Write is used by stdout and stderr

    override __.Encoding = encoding

    ///false by default
    member val Enabled = true with get, set

type ScintillaTextReader(scintilla:StandardScintilla, style:int, encoding) =
    inherit System.IO.TextReader()
    override this.ReadLine() = 
        let line : string ref = ref null
        use frm = new Form(Text="Console.Readline()", StartPosition = FormStartPosition.CenterParent)

        let tb = new TextBox(AcceptsTab=true, Dock=DockStyle.Fill)
        tb.KeyDown.Add(fun args -> 
            if args.KeyCode = Keys.Enter then 
                line := tb.Text; frm.Close()
            elif args.KeyData = (Keys.Control ||| Keys.Z) then 
                frm.Close()) //leave line null
        frm.Controls.Add(tb)

        frm.Load.Add(fun args -> frm.ClientSize <- Size(frm.ClientSize.Width, tb.Height))
        ignore <| frm.ShowDialog()
        
        let line = !line
        //echo
        scintilla.SuspendReadonly(fun () -> 
            let range = scintilla.AppendText((if line = null then "^Z" else line) + scintilla.EndOfLine.EolString)
            range.SetStyle(style))
        line

    override this.Read() = 
        let keyChar = ref 0
        use frm = new Form(Text="Console.Read()", StartPosition = FormStartPosition.CenterParent)
        let tb = new TextBox(AcceptsTab=true, Dock=DockStyle.Fill)
        tb.KeyPress.Add(fun args -> keyChar := int args.KeyChar; frm.Close())
        frm.Controls.Add(tb)
        frm.Load.Add(fun args -> frm.ClientSize <- Size(frm.ClientSize.Width, tb.Height))
        ignore <| frm.ShowDialog()
        
        //echo
        scintilla.SuspendReadonly(fun () -> 
            let range = scintilla.AppendText(!keyChar |> char |> string)
            range.SetStyle(style))
        !keyChar


module OutputScintillaStyle =
    let [<Literal>] Stdout = 0
    let [<Literal>] Stderr = 1
    let [<Literal>] Stdin = 3

///A readonly scintilla control which redirects stdout and stderr to itself (hence there should only ever be one instance of this control)
type OutputScintilla(font:Font) as this =
    inherit StandardScintilla()

    let outWriter = new ScintillaTextWriter(this, OutputScintillaStyle.Stdout, Console.OutputEncoding)
    let errWriter = new ScintillaTextWriter(this, OutputScintillaStyle.Stderr, Console.OutputEncoding)
    let inReader = new ScintillaTextReader(this, OutputScintillaStyle.Stdin, Console.InputEncoding)

    do 
        //this.IsReadOnly <- true

        let stdoutStyle = this.Styles.[OutputScintillaStyle.Stdout]
        stdoutStyle.Font <- font
        stdoutStyle.ForeColor <- System.Drawing.Color.Black

        let stdoutStyle = this.Styles.[OutputScintillaStyle.Stderr]
        stdoutStyle.Font <- font
        stdoutStyle.ForeColor <- System.Drawing.Color.DarkRed

        let stdoutStyle = this.Styles.[OutputScintillaStyle.Stdin]
        stdoutStyle.Font <- font
        stdoutStyle.ForeColor <- System.Drawing.Color.DarkGreen
        
        System.Console.SetOut(outWriter)
        System.Console.SetError(errWriter)
        System.Console.SetIn(inReader)

    member __.RedirectConsoleOutput 
        with get() = 
            outWriter.Enabled && errWriter.Enabled
        and set(value) = 
            outWriter.Enabled <- value
            errWriter.Enabled <- value