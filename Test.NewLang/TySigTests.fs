module Tests.TySigTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic
module C = Compilation

[<Fact>]
let ``non-generic`` () =
    test <@ TySig("hi",[]).Name = "hi" @>

[<Fact>]
let ``single generic arg`` () =
    test <@ TySig("hi",[TySig("bye",[])]).Name = "hi[bye]" @>

[<Fact>]
let ``two generic args`` () =
    test <@ TySig("hi",[TySig("bye",[]); TySig("night",[])]).Name = "hi[bye,night]" @>

[<Fact>]
let ``nested generic args`` () =
    test <@ TySig("hi",[TySig("bye",[TySig("night",[])]);]).Name = "hi[bye[night]]" @>