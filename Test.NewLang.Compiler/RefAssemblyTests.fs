module Tests.RefAssemblyTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic
module C = Compiler

[<Fact>]
let ``ref relative path dll`` () =
    test <@ C.eval<obj> "ref \"xunit.dll\" in Xunit.Record()" :? Xunit.Record @>

[<Fact>]
let ``ref assembly display name`` () =
    test <@ C.eval "ref \"System.Web, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a\" in open System.Web.Mail in SmtpMail.get_SmtpServer()" = "" @>

[<Fact>]
let ``connot resolve assembly`` () =
    raises<SemanticErrorException> <@ C.eval "ref \"not an assembly\" in 0" @>