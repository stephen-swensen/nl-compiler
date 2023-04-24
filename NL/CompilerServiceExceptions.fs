namespace Swensen.NL

open System

///Signal to use that a compiler service (such as eval or NLI submit) failed with one or more errors. 
///The messages passed in must contain at least one error, but may contain more.
type CompilerServiceException(msgs:CompilerMessage[]) =
    inherit Exception()
    let errors = msgs |> CompilerMessage.errors
    member this.Errors = errors
    override this.ToString() =
        sprintf "%s, errors detected:%s" (this.GetType().Name) (System.Environment.NewLine + (errors |> Seq.map string |> String.concat System.Environment.NewLine))

///Signal to user that an evaluation failed with one or more errors. The messages passed in must contain at least one error, but may contain more.
type EvaluationException(msgs) =
    inherit CompilerServiceException(msgs)

///Signal to user that an NLI submission failed with one or more errors. The messages passed in must contain at least one error, but may contain more.
type NliException(msgs) =
    inherit CompilerServiceException(msgs)