﻿module Tests.ConstructorTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

open Evaluation

[<Theory;EvalData>]
let ``constructor`` options =
    test <@ evalWith<obj> options "system.collections.arraylist()" :? System.Collections.ArrayList @>

[<Theory;EvalData>]
let ``default value of non-primitive value type`` options =
    test <@ evalWith options "biginteger()" = bigint() @>

[<Theory;EvalData>]
let ``default value of bool`` options =
    test <@ evalWith options "boolean()" = Unchecked.defaultof<bool> @>

[<Theory;EvalData>]
let ``default value of int32`` options =
    test <@ evalWith options "int32()" = Unchecked.defaultof<int32> @>

[<Theory;EvalData>]
let ``default value of double`` options =
    test <@ evalWith options "double()" = Unchecked.defaultof<double> @>

[<Theory;EvalData>]
let ``default value of char`` options =
    test <@ evalWith options "char()" = Unchecked.defaultof<char> @>

[<Theory;EvalData>]
let ``resolve simple fully qualified generic signature in constructor`` options =
    test <@ evalWith<obj> options "system.collections.generic.list[system.int32]()" :? System.Collections.Generic.List<int> @>

[<Theory;EvalData>]
let ``resolve simple non qualified generic signature in constructor`` options =
    test <@ evalWith<obj> options "list[int32]()" :? System.Collections.Generic.List<int> @>

[<Theory;EvalData>]
let ``resolve nested non qualified generic signature in constructor`` options =
    test <@ evalWith<obj> options "list[list[int32]]()" :? ResizeArray<ResizeArray<int>> @>

[<Theory;EvalData>]
let ``resolve constructor with list of generic args`` options =
    test <@ evalWith<obj> options "dictionary[string,string]()" :? Dictionary<string,string> @>

[<Theory;EvalData>]
let ``resolve complex generic signature in constructor`` options =
    test <@ evalWith<obj> options "dictionary[list[int32],dictionary[string,list[int32]]]()" :? Dictionary<ResizeArray<int>,Dictionary<string,ResizeArray<int>>> @>

[<Theory;EvalData>]
let ``Void cannot be instantiated`` options =
    raisesWith 
        <@ evalWith options "System.Void()" @>
        (expectedErrors [|14|])