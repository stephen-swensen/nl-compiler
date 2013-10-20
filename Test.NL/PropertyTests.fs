module Tests.PropertyTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open System
open Evaluation

[<Theory;EvalData>]
let ``get static property`` options =
    test <@ evalWith options "system.datetime.now" < (DateTime.Now.AddDays(1.0)) @>

[<Theory;EvalData>]
let ``set class static property`` options =
    test <@ evalWith options (Prelude.openPrefix + "NonGenericClass1.static_property_int <- 3 ; temp = NonGenericClass1.static_property_int in NonGenericClass1.static_property_int <- 0 ; temp") = 3 @>

[<Theory;EvalData>]
let ``get class instance property`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_property_int") = 0 @>

[<Theory;EvalData>]
let ``get struct instance property`` options =
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericStruct1().instance_property_int") = 0 @>

[<Theory;EvalData>]
let ``set class instance property`` options =
    test <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int <- 3 ; x.instance_property_int") = 3 @>

[<Theory;EvalData>]
let ``set struct instance property`` options =
    test <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericStruct1() in x.instance_property_int <- 3 ; x.instance_property_int ") = 3 @>

[<Theory;EvalData>]
let ``instance property has not setter`` options =
    raisesWith <@ evalWith<obj> options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int_without_setter <- 3") = null @>
        (expectedErrors [|32|])

let ``get instance property has not getter raises eval exception`` options =
    raises<EvaluationException> <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int_without_getter") @>

[<Fact (Skip="telling me not found instead of doesn't have getter")>]
let ``get instance property has not getter`` options =
    raisesWith <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int_without_getter") = 0 @>
        (expectedErrors [||])

[<Theory;EvalData>]
let ``set static_property_int_without_setter`` options =
    raisesWith <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.static_property_int_without_setter <- 3") = null @>
        (expectedErrors [|32|])

[<Theory;EvalData>]
let ``get static_property_int_without_getter raises evaluation exception`` options =
    raises<EvaluationException> <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.instance_property_int_without_getter") @>

[<Fact(Skip="todo: show better error message")>]
let ``set static_property_int_without_getter raises good error messaage`` options =
    raisesWith <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.instance_property_int_without_getter") = 0 @>
        (expectedErrors [||])

[<Theory;EvalData>] //Csharp forbids this, something we might consider
let ``inert attempt at in place mutation of struct property`` options =
    test <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_ngs1.instance_property_int <- 3; x.instance_property_ngs1.instance_property_int") = 0 @>


[<Theory;EvalData>]
let ``incompatible static property assignment type`` options =
    raisesWith <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1.static_property_int <- object()") @>
        (expectedErrors [|31|])

[<Theory;EvalData>]
let ``incompatible instance property assignment type`` options =
    raisesWith <@ evalWith options (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int <- object()") @>
        (expectedErrors [|31|])

[<Theory;EvalData>]
let ``implicit static property assignment type down cast`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_property_object <- 3; 
                     temp = Tests.NonGenericClass1.static_property_object in 
                     Tests.NonGenericClass1.static_property_object <- 0; 
                     temp[int32]") = 3 @>

[<Theory;EvalData>]
let ``implicit instance property assignment type down cast`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "x = Tests.NonGenericClass1() in
                     x.instance_property_object <- 3; 
                     x.instance_property_object[int32]") = 3 @>

[<Fact(Skip="currently not supported")>] //should have a version for instance too
let ``implicit static property assignment type coersion`` options =
    test <@ evalWith options (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_property_double <- 3; 
                     temp = Tests.NonGenericClass1.static_property_double in 
                     Tests.NonGenericClass1.static_property_double <- 0.0; 
                     temp") @>

[<Theory;EvalData>]
let ``set instance property of an expression is valid`` options = //but do we really want it to be?
    test <@ evalWith options (Prelude.openAsm + "Tests.NonGenericClass1().instance_property_int <- 0") = null @>

[<Theory;EvalData>]
let ``hide by sig`` options =
    test <@ evalWith options (openPrefix + "HideBySigClassB().Property1") = 2 @>