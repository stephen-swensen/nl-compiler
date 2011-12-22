module Tests.RefAssemblyTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``ref relative path dll`` options =
    test <@ evalWith<obj> options "open \"xunit.dll\" in Xunit.Record()" :? Xunit.Record @>

[<Theory;EvalData>]
let ``open assembly display name`` options =
    test <@ evalWith options "open @\"System.Web, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a\" in open System.Web.Mail in SmtpMail.get_SmtpServer()" = "" @>

[<Theory;EvalData>]
let ``connot resolve assembly`` options =
    raisesWith 
        <@ evalWith options "open @\"not an assembly\" in ()" @>
        (expectedErrors [|19|])

//Test.AssemblyResolveTarget.dll

[<Theory;EvalData>]
let ``get type from assembly that is not referenced in this assembly`` options =
    //for this to work do not want to copy the dll to the output directory
    test <@ evalWith<obj> options "open @\"..\\..\\Test.AssemblyResolveTarget.dll\" in Test.AssemblyResolveTarget.Class1()" <> null @>