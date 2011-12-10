module Tests.PropertyTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``get static property`` () =
    test <@ C.eval "system.datetime.now" < (DateTime.Now.AddDays(1.0)) @>

[<Fact>]
let ``get class instance property`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Test1().t1_iprop1") = 0 @>

[<Fact>]
let ``get struct instance property`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Struct1().s1_iprop1") = 0 @>