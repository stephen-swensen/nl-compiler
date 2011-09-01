namespace Swensen.NewLang

type CoreOps =
    //todo: make non-recursive
    static member Factorial(n:int) =
        let rec fact n = 
            match n with 
            | 1 | 0 -> 1
            | n     -> n * fact (n-1)
        fact n

