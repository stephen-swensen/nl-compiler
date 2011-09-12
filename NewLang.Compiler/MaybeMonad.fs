//by Tomas Petretrik at 

module Swensen.NewLang.MaybeMonad

type MaybeMonad() =
    member x.Bind(m, f) =
        match m with 
        | Some(v) -> f v 
        | None -> None
    member x.Return(v) = Some(v)

let maybe = new MaybeMonad()