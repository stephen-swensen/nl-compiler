module Swensen.NewLang.Optimization

let rec optimize (exp:texp) = 
    match exp with
    | texp.IfThenElse(condition, thenBranch, elseBranch, ty) ->
        let condition = optimize condition
        match condition with
        | texp.Bool(true) -> thenBranch
        | texp.Bool(false) -> elseBranch
        | _ -> texp.IfThenElse(condition, thenBranch, elseBranch, ty)
    | _ -> exp