namespace Swensen.NL.VisualNli
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

module OutputScintillaStyle =
    let [<Literal>] Stdout = 0
    let [<Literal>] Stderr = 1

///A readonly scintilla control which redirects stdout and stderr to itself (hence there should only ever be one instance of this control)
type OutputScintilla(font:Font) as this =
    inherit StandardScintilla()

    let outWriter = new ScintillaTextWriter(this, OutputScintillaStyle.Stdout, Console.OutputEncoding)
    let errWriter = new ScintillaTextWriter(this, OutputScintillaStyle.Stderr, Console.OutputEncoding)

    do 
        this.IsReadOnly <- true

        let stdoutStyle = this.Styles.[OutputScintillaStyle.Stdout]
        stdoutStyle.Font <- font
        stdoutStyle.ForeColor <- System.Drawing.Color.Black

        let stdoutStyle = this.Styles.[OutputScintillaStyle.Stderr]
        stdoutStyle.Font <- font
        stdoutStyle.ForeColor <- System.Drawing.Color.DarkRed
        
        System.Console.SetOut(outWriter)
        System.Console.SetError(errWriter)

    member __.RedirectConsoleOutput 
        with get() = 
            outWriter.Enabled && errWriter.Enabled
        and set(value) = 
            outWriter.Enabled <- value
            errWriter.Enabled <- value