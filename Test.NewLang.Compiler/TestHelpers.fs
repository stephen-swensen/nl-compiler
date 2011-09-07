namespace Tests

[<AutoOpen>]
module Helpers =
    let openAsm = sprintf "ref \"%s\" in " (System.Reflection.Assembly.GetExecutingAssembly().Location)
    let openNamespace = "open Tests in "
    ///prefix used to reference this assembly and this namespace in dynamic NewLang tests
    let openPrefix = openAsm + openNamespace

//need to put in namespace and not in module since can't currently resole nested classes in NewLang
type Test1() =
    member __.DoIt1<'a>() = Unchecked.defaultof<'a>
    member __.DoIt2<'a>(x:'a) = x

