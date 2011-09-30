module Tests.RefAssemblyTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic
module C = Compilation

[<Fact>]
let ``ref relative path dll`` () =
    test <@ C.eval<obj> "open \"xunit.dll\" in Xunit.Record()" :? Xunit.Record @>

[<Fact>]
let ``open assembly display name`` () =
    test <@ C.eval "open \"System.Web, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a\" in open System.Web.Mail in SmtpMail.get_SmtpServer()" = "" @>

[<Fact>]
let ``connot resolve assembly`` () =
    raisesWith 
        <@ C.eval "open \"not an assembly\" in ()" @>
        (fun (e:CompilerException) -> <@ e.CompilerError.Type = CompilerErrorType.Semantic @>)