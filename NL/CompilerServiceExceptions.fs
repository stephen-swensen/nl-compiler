namespace Swensen.NL

open System

type CompilerServiceException() =
    inherit Exception()
    let errors = MessageLogger.ActiveLogger.GetErrors()
    member this.Errors = errors
    override this.ToString() =
        sprintf "%s, errors detected:%s" (this.GetType().Name) (System.Environment.NewLine + (errors |> Seq.map string |> String.concat System.Environment.NewLine))

type EvaluationException() =
    inherit CompilerServiceException()

type NliException() =
    inherit CompilerServiceException()