module Tests.ConstructorTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic

module C = Compilation

[<Fact>]
let ``constructor`` () =
    test <@ C.eval<obj> "system.collections.arraylist()" :? System.Collections.ArrayList @>

[<Fact>]
let ``default value of non-primitive value type`` () =
    test <@ C.eval "biginteger()" = bigint() @>

[<Fact>]
let ``default value of bool`` () =
    test <@ C.eval "boolean()" = Unchecked.defaultof<bool> @>

[<Fact>]
let ``default value of int32`` () =
    test <@ C.eval "int32()" = Unchecked.defaultof<int32> @>

[<Fact>]
let ``default value of double`` () =
    test <@ C.eval "double()" = Unchecked.defaultof<double> @>

[<Fact>]
let ``default value of char`` () =
    test <@ C.eval "char()" = Unchecked.defaultof<char> @>

[<Fact>]
let ``resolve simple fully qualified generic signature in constructor`` () =
    test <@ C.eval<obj> "system.collections.generic.list[system.int32]()" :? System.Collections.Generic.List<int> @>

[<Fact>]
let ``resolve simple non qualified generic signature in constructor`` () =
    test <@ C.eval<obj> "list[int32]()" :? System.Collections.Generic.List<int> @>

[<Fact>]
let ``resolve nested non qualified generic signature in constructor`` () =
    test <@ C.eval<obj> "list[list[int32]]()" :? ResizeArray<ResizeArray<int>> @>

[<Fact>]
let ``resolve constructor with list of generic args`` () =
    test <@ C.eval<obj> "dictionary[string,string]()" :? Dictionary<string,string> @>

[<Fact>]
let ``resolve complex generic signature in constructor`` () =
    test <@ C.eval<obj> "dictionary[list[int32],dictionary[string,list[int32]]]()" :? Dictionary<ResizeArray<int>,Dictionary<string,ResizeArray<int>>> @>

[<Fact>]
let ``Void cannot be instantiated`` () =
    raisesWith 
        <@ C.eval "System.Void()" @>
        (expectedErrors [|-1|])