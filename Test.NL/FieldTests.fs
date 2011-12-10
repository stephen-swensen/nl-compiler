module Tests.FieldTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``get static field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Test1.sf") = 0 @>

[<Fact>]
let ``set static field`` () =
    //funky 'cause we need to reset sf within the test... maybe unquote needs a "teardown" verion that accepts a fun () -> _
    test <@ C.eval (Prelude.openAsm + "Tests.Test1.sf <- 3; temp = Tests.Test1.sf in Tests.Test1.sf <- 0; temp") = 3 @> //TODO ';' should bind weaker than '<-'

[<Fact>]
let ``get struct instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Struct1().s1_ifld1") = 0 @>

[<Fact>]
let ``get class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.Test3().ifld") = 0 @>