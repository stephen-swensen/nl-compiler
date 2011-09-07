namespace Tests

//need to put in namespace and not in module since can't currently resole nested classes in NewLang
type Test1() =
    member __.DoIt1<'a>() = Unchecked.defaultof<'a>
    member __.DoIt2<'a>(x:'a) = x

