module Tests.RethrowTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open System

open Evaluation

[<Theory;EvalData>]
let ``rethrow outside of catch body`` options =
    raisesWith <@ evalWith options "x = 2 in rethrow()" @>
        (expectedErrors [|51|])

[<Theory;EvalData>]
let ``rethrow in catch`` options =
    raises<ArgumentException> <@ evalWith options "try { throw(argumentexception()) } catch { rethrow() }" @>

[<Theory;EvalData>]
let ``nested rethrow`` options =
    test <@ evalWith options "try { throw(argumentexception()); -1 } catch { try { rethrow(); 0 } catch { 1 } }" = 1 @>

[<Theory;EvalData>]
let ``nested rethrow in finally`` options =
    raises<ArgumentException> <@ evalWith options "try { throw(argumentexception()) } catch { try { () } catch { () } finally { rethrow() } }" @>