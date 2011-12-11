module Tests.FieldTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``get static field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1.static_field_int") = 0 @>

[<Fact>]
let ``set static field`` () =
    //funky 'cause we need to reset sf within the test... maybe unquote needs a "teardown" verion that accepts a fun () -> _
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1.static_field_int <- 3; temp = Tests.NonGenericClass1.static_field_int in Tests.NonGenericClass1.static_field_int <- 0; temp") = 3 @> //TODO ';' should bind weaker than '<-'

[<Fact>]
let ``get struct instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericStruct1().instance_field_int") = 0 @>

[<Fact(Skip="not implemented")>]
let ``set struct instance field`` () =
    test <@ C.eval (Prelude.openAsm + "ngs = Tests.NonGenericStruct1() in ngs.instance_field_int <- 3; ngs.instance_field_int") = 3 @>

[<Fact>]
let ``get class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_int") = 0 @>