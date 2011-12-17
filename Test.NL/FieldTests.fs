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

[<Fact>] //verifies IL standard optimization
let ``set static field with default of value type`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1.static_field_decimal3 <- default[decimal]; temp = Tests.NonGenericClass1.static_field_decimal3 in Tests.NonGenericClass1.static_field_decimal3 <- decimal(3); temp") = 0M @> //TODO ';' should bind weaker than '<-'

[<Fact>]
let ``get struct instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericStruct1().instance_field_int") = 0 @>

[<Fact>]
let ``assign struct instance field to var`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericStruct1().instance_field_int in x") = 0 @>

[<Fact>]
let ``get class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_int") = 0 @>

[<Fact>]
let ``set struct instance field`` () =
    test <@ C.eval (Prelude.openAsm + "ngs = Tests.NonGenericStruct1() in ngs.instance_field_int <- 3; ngs.instance_field_int") = 3 @>

[<Fact>]
let ``set class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "ngc = Tests.NonGenericClass1() in ngc.instance_field_int <- 3; ngc.instance_field_int") = 3 @>

[<Fact>]
let ``set class instance field with default of value type`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_decimal3 <- default[decimal]; x.instance_field_decimal3") = 0M @>

[<Fact>]
let ``in place mutation of a static class field`` () =
    test <@ C.eval (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_ngc1.instance_field_int <- 3; 
                     temp = Tests.NonGenericClass1.static_field_ngc1.instance_field_int in
                     Tests.NonGenericClass1.static_field_ngc1.instance_field_int <- 0;
                     temp") = 3 @>

[<Fact>]
let ``in place mutation of a static struct field`` () =
    test <@ C.eval (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_ngs1.instance_field_int <- 3; 
                     temp = Tests.NonGenericClass1.static_field_ngs1.instance_field_int in
                     Tests.NonGenericClass1.static_field_ngs1.instance_field_int <- 0;
                     temp") = 3 @>

[<Fact>]
let ``in place mutation of a instance class field`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_ngc2.instance_field_int <- 3; x.instance_field_ngc2.instance_field_int") = 3 @>

[<Fact>]
let ``in place mutation of a instance struct field`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_ngs1.instance_field_int <- 3; x.instance_field_ngs1.instance_field_int") = 3 @>

[<Fact>]
let ``incompatible static field assignment type`` () =
    raisesWith <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1.static_field_int <- object()") @>
        (expectedErrors [|30|])

[<Fact>]
let ``incompatible instance field assignment type`` () =
    raisesWith <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_int <- object()") @>
        (expectedErrors [|30|])

[<Fact>]
let ``implicit static field assignment type down cast`` () =
    test <@ C.eval (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_object <- 3; 
                     temp = Tests.NonGenericClass1.static_field_object in 
                     Tests.NonGenericClass1.static_field_object <- 0; 
                     temp[int32]") = 3 @>

[<Fact>]
let ``implicit instance field assignment type down cast`` () =
    test <@ C.eval (Prelude.openAsm + 
                    "x = Tests.NonGenericClass1() in
                     x.instance_field_object <- 3; 
                     x.instance_field_object[int32]") = 3 @>

[<Fact(Skip="currently not supported")>] //should have a version for instance too
let ``implicit static field assignment type coersion`` () =
    test <@ C.eval (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_double <- 3; 
                     temp = Tests.NonGenericClass1.static_field_double in 
                     Tests.NonGenericClass1.static_field_double <- 0.0; 
                     temp") @>

[<Fact>]
let ``set instance field of an expression is valid`` () = //but do we really want it to be?
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_int <- 0") = null @>