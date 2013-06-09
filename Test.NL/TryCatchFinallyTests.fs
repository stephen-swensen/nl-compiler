module Tests.TryCatchFinallyTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

open Evaluation

[<Theory;EvalData>]
let ``try catch finally - mismatch ret tys`` options =
    raisesWith <@ evalWith options "try { type[string] } catch { 1 }" @>
        (expectedErrors [|49|])

[<Theory;EvalData>]
let ``try catch finally - mismatch ret tys - void in try block`` options =
    raisesWith <@ evalWith options "try { () } catch { 1 }" @>
        (expectedErrors [|49|])

[<Theory;EvalData>]
let ``try catch finally - mismatch ret tys - void in catch block`` options =
    raisesWith <@ evalWith options "try { 1 } catch { () }" @>
        (expectedErrors [|49|])

[<Theory;EvalData>]
let ``try catch finally - no ty filter, no var, no exn`` options =
    test <@ evalWith options "try { 0 } catch { 1 }" = 0 @>

[<Theory;EvalData>]
let ``try catch finally - no ty filter, no var, yes exn`` options =
    test <@ evalWith options "try { throw(exception()) } catch { 1 }" = 1 @>

[<Theory;EvalData>]
let ``try catch finally - yes ty filter, yes var, yes exn`` options =
    test <@ evalWith options "try { throw(exception()) } catch[exception] x { 1 }" = 1 @>

[<Theory;EvalData>]
let ``try catch finally - yes ty filter, no var, yes exn`` options =
    test <@ evalWith options "try { throw(exception()) } catch[exception] { 1 }" = 1 @>

[<Theory;EvalData>]
let ``try catch finally - yes ty filter, yes var, yes exn, access caught exn`` options =
    test <@ evalWith options "try { throw(exception()); type[string] } catch[exception] x { x.gettype() }" = typeof<exn> @>

[<Theory;EvalData>]
let ``try catch finally - no ty filter, yes var, yes exn, access caught exn`` options =
    test <@ evalWith options "try { throw(exception()); type[string] } catch x { x.gettype() }" = typeof<exn> @>

[<Theory;EvalData>]
let ``try catch finally - two catch blocks, second catches exn`` options =
    test <@ evalWith options "try { throw(exception()) } catch[argumentexception] x { 1 } catch { 2 }" = 2 @>

[<Theory;EvalData>]
let ``try catch finally - two catch blocks, first catches exn`` options =
    test <@ evalWith options "try { throw(argumentexception()) } catch[argumentexception] x { 1 } catch { 2 }" = 1 @>

[<Theory;EvalData>]
let ``try catch finally - final catch unreachable `` options =
    use sink = new BasicMessageSink()
    test <@ evalWith options "try { throw(argumentexception()) } catch { 1 } catch[argumentexception] x { 2 }" = 1 @>;
    test <| expectedWarnings [|17|] (sink.GetMessages((=)MessageLevel.Warning))

[<Theory;EvalData>]
let ``try catch finally - finally is called`` options =
    test <@ evalWith options "x = 0 in try { 0 } catch{ 1 } finally { x <- 2 }; x" = 2 @>

[<Theory;EvalData>]
let ``try catch finally - either catch or finally required`` options =
    raisesWith <@ evalWith options "try { 0 }" = 0 @>
        (expectedErrors [|50|])

[<Theory;EvalData>]
let ``int type in try block and Escape type in catch block`` options =
    test <@ evalWith options "try { 1 } catch { rethrow() }" = 1 @>
