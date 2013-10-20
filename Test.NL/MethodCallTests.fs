module Tests.MethodCallTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``instance call on value type`` options =
    test <@ evalWith options "3.ToString()" = "3" @>

[<Theory;EvalData>]
let ``instance calls are case insensitive`` options =
    test <@ evalWith options "3.tostrINg()" = "3" @>

[<Theory;EvalData>]
let ``instance call on obj type`` options =
    test <@ evalWith options "\"3\".ToString()" = "3" @>

[<Theory;EvalData>]
let ``static call`` options =
    test <@ evalWith options "System.String.Concat(\"hello \", \"world\")" = "hello world" @>

[<Theory;EvalData>]
let ``static call is case insensitive`` options =
    test <@ evalWith options "System.STRING.Concat(\"hello \", \"world\")" = "hello world" @>

[<Theory;EvalData>]
let ``implicit downcast ref type and value type static call args`` options =
    //resolves to String.concat(obj,obj)
    test <@ evalWith options "\"asdf\" + 3" = "asdf3" @>

[<Theory;EvalData>]
let ``call non-generic static method on generic type`` options =
    test <@ evalWith<obj> options "HashSet[string].CreateSetComparer()" :? IEqualityComparer<HashSet<string>> @>

[<Theory;EvalData>]
let ``call generic static method on generic type`` options =
    test <@ evalWith options (openPrefix + "StaticGenericClass1[string].StaticGenericMethod[int32]()") = 0 @>

[<Theory;EvalData>]
let ``call generic static method on invalid generic type`` options =
    raisesWith 
        <@ evalWith options (openPrefix + "INVALID_TYPE[string].StaticGenericMethod[int32]()") = 0 @>
        (expectedErrors [|1|])

[<Theory;EvalData>]
let ``call invalid generic static method on generic type`` options =
    raisesWith 
        <@ evalWith options (openPrefix + "StaticGenericClass1[string].INVALID_METHOD[int32]()") = 0 @>
        (expectedErrors [|11|])

[<Theory;EvalData>]
let ``call generic static method with explicit generic args and no type-wise overloads on non-generic static type`` options = //these could be inferable
    test <@ evalWith options "tuple.create[int32,datetime](3, datetime())" = (3, System.DateTime()) @>

[<Theory;EvalData>]
let ``call generic instance method with explicit generic args and no overloads on var`` options =
    test <@ evalWith options (openPrefix + "x = NonGenericClass1() in x.InstanceGenericMethod[int32](1)") = 1 @>

[<Theory;EvalData>]
let ``call generic instance method with explicit generic args and no overloads on expression`` options =
    test <@ evalWith options (openPrefix + "NonGenericClass1().InstanceGenericMethod[int32](1)") = 1 @>

[<Theory;EvalData>]
let ``call generic instance method with invalid generic type args`` options =
    raisesWith 
        <@ evalWith options (openPrefix + "NonGenericClass1().InstanceGenericMethod[INVALID](1)")@>
        (expectedErrors [|1|])

[<Theory;EvalData>]
let ``call generic instance method with 2 of 3 invalid generic type args`` options =
    raisesWith 
        <@ evalWith options (openPrefix + "NonGenericClass1().InstanceGenericMethodWithThreeTypeArgs[INVALID1, int32, INVALID2]()")@>
        (expectedErrors [|1;1|])

[<Theory;EvalData>]
let ``hide by sig`` options =
    test <@ evalWith options (openPrefix + "HideBySigClassB().Method1(0)") = 2 @>

