module Tests.ThrowTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open System

open Evaluation

[<Theory;EvalData>]
let ``throw in let`` options =
    raises<ArgumentException> <@ evalWith options "x = throw(argumentexception()); 0 in x" @>

[<Theory;EvalData>]
let ``throw with default overload`` options =
    raises<System.Exception> <@ evalWith options "throw(exception())" @>

[<Theory;EvalData>]
let ``throw specific with default overload`` options =
    raises<System.ArgumentException> <@ evalWith options "throw(ArgumentException())" @>

[<Theory;EvalData>]
let ``throw with non default overload`` options =
    raisesWith<System.Exception> <@ evalWith options "throw(exception(\"hi\"))" @>
        (fun x -> <@ x.Message = "hi" @>)

[<Theory;EvalData>]
let ``throw with non default overload and compound expression`` options =
    raisesWith<System.Exception> <@ evalWith options "throw(x = \"hi\" in exception(x))" @>
        (fun x -> <@ x.Message = "hi" @>)

[<Theory;EvalData>]
let ``unreachable throw warning`` options =
    use sink = new BasicMessageSink()
    try 
        evalWith options "throw(exception()); ()"
    with _ -> ()
    test <| expectedWarnings [|17|] (sink.GetMessages((=)MessageLevel.Warning))