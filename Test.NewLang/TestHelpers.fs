namespace Tests

[<AutoOpen>]
module Helpers =
    ///code fragment to reference this assembly within tests so we can test with types defined in this assembly
    let openAsm = sprintf "open \"%s\" in " (System.Reflection.Assembly.GetExecutingAssembly().Location)
    ///code fragment to open the root namespace of this assembly for testing with types defined within this assembly
    let openNamespace = "open Tests in "
    ///prefix used to reference this assembly and this namespace in dynamic NewLang tests
    let openPrefix = openAsm + openNamespace

type Test1() =
    member __.DoIt1<'a>() = Unchecked.defaultof<'a>
    member __.DoIt2<'a>(x:'a) = x

type Test2<'a> =
    static member DoIt1<'b>() = Unchecked.defaultof<'b>

