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
let ``set class static property`` () =
    test <@ C.eval (Prelude.openPrefix + "NonGenericClass1.static_property_int <- 3 ; temp = NonGenericClass1.static_property_int in NonGenericClass1.static_property_int <- 0 ; temp") = 3 @>

[<Fact>]
let ``get class instance property`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_property_int") = 0 @>

[<Fact>]
let ``get struct instance property`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericStruct1().instance_property_int") = 0 @>

[<Fact>]
let ``set class instance property`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int <- 3 ; x.instance_property_int") = 3 @>

[<Fact>]
let ``set struct instance property`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericStruct1() in x.instance_property_int <- 3 ; x.instance_property_int ") = 3 @>

[<Fact>]
let ``instance property has not setter`` () =
    raisesWith <@ C.eval<obj> (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int_without_setter <- 3") = null @>
        (expectedErrors [|32|])

[<Fact (Skip="telling me not found instead of doesn't have getter")>]
let ``instance property has not getter`` () =
    raisesWith <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_int_without_getter") = 0 @>
        (expectedErrors [||])

[<Fact>]
let ``static_property_int_without_setter`` () =
    raisesWith <@ C.eval<obj> (Prelude.openAsm + "Tests.NonGenericClass1.static_property_int_without_setter <- 3") = null @>
        (expectedErrors [|32|])

[<Fact(Skip="todo: show better error message")>]
let ``static_property_int_without_getter`` () =
    raisesWith <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1.instance_property_int_without_getter") = 0 @>
        (expectedErrors [||])