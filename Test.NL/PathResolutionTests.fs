module Tests.PathResolutionTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``get static then class instance then class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Test1.sfld.ifld.ifld") = 0 @>

[<Fact>]
let ``expr get class instance then class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Test1().ifld.ifld") = 0 @>

[<Fact>]
let ``expr get class property then struct property`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Test1().t1_iprop2.s1_iprop1") = 0 @>