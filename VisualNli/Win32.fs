namespace Swensen.NL.VisualNli
open System.Runtime.InteropServices
open System
open System.Windows.Forms

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

module Control =
    let inline update< ^a when ^a :(member BeginUpdate: unit -> unit) 
                       and ^a : (member EndUpdate: unit -> unit)> (this: ^a) f =
        (^a : (member BeginUpdate: unit -> unit) (this))
        f()
        (^a : (member EndUpdate: unit -> unit) (this))