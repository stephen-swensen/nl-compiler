namespace Swensen.NL.VisualNli
open System
open System.Drawing
open System.Windows.Forms
open ScintillaNET
open Swensen.NL
open System.Text.RegularExpressions

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

    ///true by default
    member val Enabled = true with get, set

type internal ReadBuffer(?buff: int[]) =
    let buff = defaultArg buff [||]

    let mutable pos : int = 0
    let next() = 
        if pos < buff.Length then
            pos <- pos+1
            Some(buff.[pos-1])
        else
            None
    member __.Next() = next()
    member __.Length = buff.Length

type ScintillaTextReader(scintilla:StandardScintilla, style:int, encoding) =
    inherit System.IO.TextReader()

    ///used by Read()
    let mutable rbuff = ReadBuffer()
    let eol = scintilla.EndOfLine.EolString

    let echo out = //echo
        scintilla.SuspendReadonly(fun () -> 
            let range = scintilla.AppendText(out)
            range.SetStyle(style))

    ///http://msdn.microsoft.com/en-us/library/system.console.readline.aspx using modal dialog for blocking
    override this.ReadLine() = 
        let line : string ref = ref null
        use frm = new Form(Text="Console.Readline()", StartPosition = FormStartPosition.CenterParent)

        let tb = new TextBox(AcceptsTab=true, Dock=DockStyle.Fill)
        tb.KeyDown.Add(fun args -> 
            if args.KeyCode = Keys.Enter then 
                echo (tb.Text + eol)
                line := tb.Text
                frm.Close()
            elif args.KeyData = (Keys.Control ||| Keys.Z) then 
                echo ("^Z" + eol)
                frm.Close()) //leave line null
        frm.Controls.Add(tb)

        frm.Load.Add(fun args -> frm.ClientSize <- Size(frm.ClientSize.Width, tb.Height))
        ignore <| frm.ShowDialog()
        
        !line

    ///http://msdn.microsoft.com/en-us/library/system.console.read.aspx using modal dialog for blocking when needed (buff empty)
    override this.Read() = 
        match rbuff.Next() with
        | Some(cint) -> cint
        | None ->
            use frm = new Form(Text="Console.Read()", StartPosition = FormStartPosition.CenterParent)
            let tb = new TextBox(AcceptsTab=true, Dock=DockStyle.Fill)
            tb.KeyDown.Add(fun args -> 
                if args.KeyCode = Keys.Enter then 
                    echo (tb.Text + eol)
                    rbuff <- ReadBuffer((tb.Text + eol).ToCharArray() |> Array.map (fun c -> int c))
                    frm.Close()
                elif args.KeyData = (Keys.Control ||| Keys.Z) then 
                    echo ("^Z" + eol)
                    rbuff <- ReadBuffer([|-1|])
                    frm.Close())
            frm.Controls.Add(tb)
            frm.Load.Add(fun args -> frm.ClientSize <- Size(frm.ClientSize.Width, tb.Height))
            ignore <| frm.ShowDialog()
            assert (rbuff.Length > 0)
            this.Read()


module OutputScintillaStyle =
    let [<Literal>] Stdout = 0
    let [<Literal>] Stderr = 1
    let [<Literal>] Stdin = 2

    let [<Literal>] CompilerWarning = 3
    let [<Literal>] CompilerError = 4

///A readonly scintilla control which redirects stdout and stderr to itself (hence there should only ever be one instance of this control)
type OutputScintilla(font:Font) as this =
    inherit StandardScintilla()

    let compilerMessageDoubleClick = new Event<int * int>()
    
    do this.DoubleClick.Add <| fun _ ->
        let pos = this.CurrentPos
        let line = this.Lines.FromPosition(pos)
        
        let text = line.Text
        match text with
        | Regex.Compiled.Match @"^\|(Semantic|Syntactic) (error|warning) \(NL[0-9]+\) from \(([0-9]+),([0-9]+)\).*" {GroupValues=[_;_;Int32(linePos);Int32(colPos)]} ->
            this.Selection.Clear() |> ignore
            //scintilla starts counting at 0 for both line and pos, adjust for our scintilla-interested event listeners
            compilerMessageDoubleClick.Trigger(linePos-1,colPos-1)
        | _ -> ()

    let outWriter = new ScintillaTextWriter(this, OutputScintillaStyle.Stdout, Console.OutputEncoding)
    let errWriter = new ScintillaTextWriter(this, OutputScintillaStyle.Stderr, Console.OutputEncoding)
    let inReader = new ScintillaTextReader(this, OutputScintillaStyle.Stdin, Console.InputEncoding)

    do 
        this.IsReadOnly <- true

        let configureStyle (styleIndex:int, color:System.Drawing.Color) =
            let style = this.Styles.[styleIndex]
            style.Font <- font
            style.ForeColor <- color

        let styleConfig = [
            OutputScintillaStyle.Stdout, Color.Black
            OutputScintillaStyle.Stderr, Color.Red
            OutputScintillaStyle.Stdin, Color.Green
            OutputScintillaStyle.CompilerWarning, Color.Orange
            OutputScintillaStyle.CompilerError, Color.Red
        ]

        do styleConfig |> Seq.iter configureStyle
        
        System.Console.SetOut(outWriter)
        System.Console.SetError(errWriter)
        System.Console.SetIn(inReader)

    member __.CompilerMessageDoubleClick = compilerMessageDoubleClick.Publish

    member __.ConsoleOutEnabled
        with get() = 
            outWriter.Enabled
        and set(value) = 
            outWriter.Enabled <- value

    member __.ConsoleErrorEnabled
        with get() = 
            errWriter.Enabled
        and set(value) = 
            errWriter.Enabled <- value

    member __.Sink(msg:CompilerMessage) =
        let msgText = sprintf "|%O|" msg
        this.SuspendReadonly(fun() ->
            let range = this.AppendText(msgText + this.EndOfLine.EolString)
            let styleIndex = 
                match msg.Level with 
                | Error -> OutputScintillaStyle.CompilerError 
                | Warning -> OutputScintillaStyle.CompilerWarning
            range.SetStyle(styleIndex)
            this.Scrolling.ScrollBy(0, this.Lines.Count)
            this.Update()
        )