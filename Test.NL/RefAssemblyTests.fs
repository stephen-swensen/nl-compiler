module Tests.RefAssemblyTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``ref relative path dll`` options =
    test <@ evalWith<obj> options "open \"xunit.dll\" in Xunit.Record()" :? Xunit.Record @>

[<Theory;EvalData>]
let ``open assembly display name`` options =
    test <@ evalWith options "open @\"System.Net.Mail, Version=7.0.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51\" in open System.Net.Mail in MailMessage().get_From()" = null @>

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