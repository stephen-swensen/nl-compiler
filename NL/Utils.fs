namespace Swensen.NL

module Option =
    ///Convert a type which allows null to an option type
    let ofAllowsNull nullable =
        match nullable with
        | null -> None
        | _ -> Some(nullable)

module Seq =
    ///cons x with xs
    let cons x xs =
        seq { yield x ; yield! xs}