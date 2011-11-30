module Tests.TySigTests

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic
module C = Compilation

[<Fact>]
let ``non-generic`` () =
    test <@ Ast.TySig("hi",[]).Name = "hi" @>

[<Fact>]
let ``single generic arg`` () =
    test <@ Ast.TySig("hi",[Ast.TySig("bye",[])]).Name = "hi[bye]" @>

[<Fact>]
let ``two generic args`` () =
    test <@ Ast.TySig("hi",[Ast.TySig("bye",[]); Ast.TySig("night",[])]).Name = "hi[bye,night]" @>

[<Fact>]
let ``nested generic args`` () =
    test <@ Ast.TySig("hi",[Ast.TySig("bye",[Ast.TySig("night",[])]);]).Name = "hi[bye[night]]" @>