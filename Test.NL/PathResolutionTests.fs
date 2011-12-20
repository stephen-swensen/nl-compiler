module Tests.PathResolutionTests

open Xunit;; open Xunit.Extensions

open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
open Evaluation

[<Theory;EvalData>]
let ``get static then class instance then class instance field`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.static_field_ngc1.instance_field_ngc2.instance_field_int") = 0 @>

[<Theory;EvalData>]
let ``expr get class instance then class instance field`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngc2.instance_field_int") = 0 @>

[<Theory;EvalData>]
let ``expr get class property then struct property`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_property_ngs1.instance_property_int") = 0 @>

[<Theory;EvalData>]
let ``expr get class field then call method`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngc2.InstanceNonGenericMethod()") = 0 @>

[<Theory;EvalData>]
let ``expr get class field then call method with invalid method name`` options =
    raisesWith 
        <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngc2.INVALID_METHOD_NAME()") = 0 @>
        (expectedErrors [|10|])

[<Theory;EvalData>]
let ``expr get class field then get class property then call method`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngc2.instance_property_ngs1.InstanceNonGenericMethod()") = 0 @>

[<Theory;EvalData>]
let ``set instance field of field of an expression is valid`` options = //but do we really want it to be?
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngs1.instance_field_int <- 0") ; true @>

[<Theory;EvalData>]
let ``instance field or property get on expression not found`` options = //but do we really want it to be?
    raisesWith 
        <@ evalWith options (Prelude.openPrefix + "NonGenericClass1().INVALID_FIELD_OR_PROPERTY") @>
        (expectedErrors [|29|])

[<Theory;EvalData>]
let ``instance method of a var`` options =
    test <@ evalWith options (Prelude.openPrefix + "x = NonGenericClass1() in x.InstanceNonVoidMethod()") = 0 @>

[<Theory;EvalData>]
let ``instance method of a vars instance field`` options =
    test <@ evalWith options (Prelude.openPrefix + "x = NonGenericClass1() in x.instance_field_int.ToString()") = "0" @>

[<Theory;EvalData>]
let ``instance method of a vars instance property`` options =
    test <@ evalWith options (Prelude.openPrefix + "x = NonGenericClass1() in x.instance_property_int.ToString()") = "0" @>

[<Theory;EvalData>]
let ``instance method of static field`` options =
    test <@ evalWith options (Prelude.openPrefix + "NonGenericClass1.static_field_int.ToString()") = "0" @>

[<Theory;EvalData>]
let ``instance method of static property`` options =
    test <@ evalWith options (Prelude.openPrefix + "NonGenericClass1.static_property_int.ToString()") = "0" @>

[<Theory;EvalData>]
let ``instance method of static fields instance field`` options =
    test <@ evalWith options (Prelude.openPrefix + "NonGenericClass1.static_field_ngs1.instance_field_int.ToString()") = "0" @>

[<Theory;EvalData>]
let ``instance method of static property not found`` options = //but do we really want it to be?
    raisesWith 
        <@ evalWith options (Prelude.openPrefix + "NonGenericClass1.static_property_int.INVALID()") @>
        (expectedErrors [|10|])

[<Theory;EvalData>]
let ``long var name instance call`` options =
    test <@ evalWith options (Prelude.openPrefix + "x.x.x.x = 0 in x.x.x.x.tostring()") = "0" @>