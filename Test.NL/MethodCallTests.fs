module Tests.MethodCallTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
module C = Compilation

[<Fact>]
let ``instance call on value type`` () =
    test <@ C.eval "3.ToString()" = "3" @>

[<Fact>]
let ``instance calls are case insensitive`` () =
    test <@ C.eval "3.tostrINg()" = "3" @>

[<Fact>]
let ``instance call on obj type`` () =
    test <@ C.eval "\"3\".ToString()" = "3" @>

[<Fact>]
let ``static call`` () =
    test <@ C.eval "System.String.Concat(\"hello \", \"world\")" = "hello world" @>

[<Fact>]
let ``static call is case insensitive`` () =
    test <@ C.eval "System.STRING.Concat(\"hello \", \"world\")" = "hello world" @>

[<Fact>]
let ``implicit downcast ref type and value type static call args`` () =
    //resolves to String.concat(obj,obj)
    test <@ C.eval "\"asdf\" + 3" = "asdf3" @>

[<Fact>]
let ``call non-generic static method on generic type`` () =
    test <@ C.eval<obj> "HashSet[string].CreateSetComparer()" :? IEqualityComparer<HashSet<string>> @>

[<Fact>]
let ``call generic static method on generic type`` () =
    test <@ C.eval (openPrefix + "StaticGenericClass1[string].StaticGenericMethod[int32]()") = 0 @>

[<Fact>]
let ``call generic static method on invalid generic type`` () =
    raisesWith 
        <@ C.eval (openPrefix + "INVALID_TYPE[string].StaticGenericMethod[int32]()") = 0 @>
        (expectedErrors [|1|])

[<Fact>]
let ``call invalid generic static method on generic type`` () =
    raisesWith 
        <@ C.eval (openPrefix + "StaticGenericClass1[string].INVALID_METHOD[int32]()") = 0 @>
        (expectedErrors [|11|])

[<Fact>]
let ``call generic static method with explicit generic args and no type-wise overloads on non-generic static type`` () = //these could be inferable
    test <@ C.eval "tuple.create[int32,datetime](3, datetime())" = (3, System.DateTime()) @>

[<Fact>]
let ``call generic instance method with explicit generic args and no overloads on var`` () =
    test <@ C.eval (openPrefix + "x = NonGenericClass1() in x.InstanceGenericMethod[int32](1)") = 1 @>

[<Fact>]
let ``call generic instance method with explicit generic args and no overloads on expression`` () =
    test <@ C.eval (openPrefix + "NonGenericClass1().InstanceGenericMethod[int32](1)") = 1 @>

[<Fact>]
let ``call generic instance method with invalid generic type args`` () =
    raisesWith 
        <@ C.eval (openPrefix + "NonGenericClass1().InstanceGenericMethod[INVALID](1)")@>
        (expectedErrors [|1|])

[<Fact>]
let ``call generic instance method with 2 of 3 invalid generic type args`` () =
    raisesWith 
        <@ C.eval (openPrefix + "NonGenericClass1().InstanceGenericMethodWithThreeTypeArgs[INVALID1, int32, INVALID2]()")@>
        (expectedErrors [|1;1|])