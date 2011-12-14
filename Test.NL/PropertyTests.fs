﻿module Tests.PropertyTests

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
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_property_int") = 0 @>

[<Fact>]
let ``get struct instance property`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericStruct1().instance_property_int") = 0 @>

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

[<Fact>]
let ``static_property_int_without_getter`` () =
    raisesWith <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1.instance_property_int_without_getter") = 0 @>
        (expectedErrors [||])