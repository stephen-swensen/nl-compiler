module Tests.PathResolutionTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
open System
module C = Compilation

[<Fact>]
let ``get static then class instance then class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1.static_field_ngc1.instance_field_ngc2.instance_field_int") = 0 @>

[<Fact>]
let ``expr get class instance then class instance field`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngc2.instance_field_int") = 0 @>

[<Fact>]
let ``expr get class property then struct property`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_property_ngs1.instance_property_int") = 0 @>

[<Fact>]
let ``expr get class field then call method`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngc2.InstanceNonGenericMethod()") = 0 @>

[<Fact>]
let ``expr get class field then get class property then call method`` () =
    test <@ C.eval (Prelude.openAsm + "Tests.NonGenericClass1().instance_field_ngc2.instance_property_ngs1.InstanceNonGenericMethod()") = 0 @>