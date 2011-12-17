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

[<Fact>]
let ``get static then class instance then class instance field then set`` () =
    test <@ C.eval (Prelude.openAsm + 
                    "Tests.NonGenericClass1.static_field_ngc1.instance_field_ngc2.instance_field_int <- 3; 
                     temp = Tests.NonGenericClass1.static_field_ngc1.instance_field_ngc2.instance_field_int in
                     Tests.NonGenericClass1.static_field_ngc1.instance_field_ngc2.instance_field_int <- 0;
                     temp") = 3 @>

[<Fact>]
let ``var get class instance field then class instance field then set`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_field_ngc2.instance_field_int <- 3; x.instance_field_ngc2.instance_field_int") = 3 @>

[<Fact>]
let ``var get class property then struct property then set`` () =
    test <@ C.eval (Prelude.openAsm + "x = Tests.NonGenericClass1() in x.instance_property_ngs1.instance_property_int <- 3; x.instance_property_ngs1.instance_property_int") = 3 @>