module Tests.FieldTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open System
open Evaluation

[<Theory;EvalData>]
let ``get static field`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.static_field_int") = 0 @>

[<Theory;EvalData>]
let ``set static field`` options =
    //funky 'cause we need to reset sf within the test... maybe unquote needs a "teardown" verion that accepts a fun () -> _
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.static_field_int <- 3; temp = Tests.NonGenericClass1.static_field_int in Tests.NonGenericClass1.static_field_int <- 0; temp") = 3 @> //TODO ';' should bind weaker than '<-'

[<Theory;EvalData>] //verifies IL standard optimization
let ``set static field with default of value type`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.static_field_decimal3 <- default[decimal]; temp = Tests.NonGenericClass1.static_field_decimal3 in Tests.NonGenericClass1.static_field_decimal3 <- decimal(3); temp") = 0M @> //TODO ';' should bind weaker than '<-'

[<Theory;EvalData>]
let ``get struct instance field`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericStruct1().instance_field_int") = 0 @>

[<Theory;EvalData>]
let ``assign struct instance field to var`` options =
    test <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericStruct1().instance_field_int in x") = 0 @>

[<Theory;EvalData>]
let ``get class instance field`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_int") = 0 @>

[<Theory;EvalData>]
let ``set struct instance field`` options =
    test <@ evalWith options (Prelude.openAsm + "ngs = Tests.NonGenericStruct1() in ngs.instance_field_int <- 3; ngs.instance_field_int") = 3 @>

[<Theory;EvalData>]
let ``set class instance field`` options =
    test <@ evalWith options (Prelude.openAsm + "ngc = Tests.NonGenericClass1() in ngc.instance_field_int <- 3; ngc.instance_field_int") = 3 @>

[<Theory;EvalData>]
let ``set class instance field with default of value type`` options =
    test <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_decimal3 <- default[decimal]; x.instance_field_decimal3") = 0M @>

[<Theory;EvalData>]
let ``in place mutation of a static class field`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_ngc1.instance_field_int <- 3; 
                     temp = Tests.NonGenericClass1.static_field_ngc1.instance_field_int in
                     Tests.NonGenericClass1.static_field_ngc1.instance_field_int <- 0;
                     temp") = 3 @>

[<Theory;EvalData>]
let ``in place mutation of a static struct field`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_ngs1.instance_field_int <- 3; 
                     temp = Tests.NonGenericClass1.static_field_ngs1.instance_field_int in
                     Tests.NonGenericClass1.static_field_ngs1.instance_field_int <- 0;
                     temp") = 3 @>

[<Theory;EvalData>]
let ``in place mutation of a instance class field`` options =
    test <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_ngc2.instance_field_int <- 3; x.instance_field_ngc2.instance_field_int") = 3 @>

[<Theory;EvalData>]
let ``in place mutation of a instance struct field`` options =
    test <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_ngs1.instance_field_int <- 3; x.instance_field_ngs1.instance_field_int") = 3 @>

[<Theory;EvalData>]
let ``incompatible static field assignment type`` options =
    raisesWith <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.static_field_int <- object()") @>
        (expectedErrors [|30|])

[<Theory;EvalData>]
let ``incompatible instance field assignment type`` options =
    raisesWith <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_int <- object()") @>
        (expectedErrors [|30|])

[<Theory;EvalData>]
let ``implicit static field assignment type down cast`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_object <- 3; 
                     temp = Tests.NonGenericClass1.static_field_object in 
                     Tests.NonGenericClass1.static_field_object <- 0; 
                     temp[int32]") = 3 @>

[<Theory;EvalData>]
let ``implicit instance field assignment type down cast`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "x = Tests.NonGenericClass1() in
                     x.instance_field_object <- 3; 
                     x.instance_field_object[int32]") = 3 @>

[<Fact(Skip="currently not supported")>] //should have a version for instance too
let ``implicit static field assignment type coersion`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_double <- 3; 
                     temp = Tests.NonGenericClass1.static_field_double in 
                     Tests.NonGenericClass1.static_field_double <- 0.0; 
                     temp") @>

[<Theory;EvalData>]
let ``set instance field of an expression is valid`` options = //but do we really want it to be?
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_int <- 0") = null @>