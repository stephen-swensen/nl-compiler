module Tests.TySigTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open Evaluation

let pos:PositionRange = Unchecked.defaultof<PositionRange>

[<Fact>]
let ``non-generic`` () =
    test <@ Ast.TySig("hi",[], pos).Name = "hi" @>

[<Fact>]
let ``single generic arg`` () =
    test <@ Ast.TySig("hi",[Ast.TySig("bye",[], pos)], pos).Name = "hi[bye]" @>

[<Fact>]
let ``two generic args`` () =
    test <@ Ast.TySig("hi",[Ast.TySig("bye",[], pos); Ast.TySig("night",[], pos)], pos).Name = "hi[bye,night]" @>

[<Fact>]
let ``nested generic args`` () =
    test <@ Ast.TySig("hi",[Ast.TySig("bye",[Ast.TySig("night",[], pos)], pos);], pos).Name = "hi[bye[night]]" @>