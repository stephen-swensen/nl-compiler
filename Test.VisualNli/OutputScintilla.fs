module Test.VisualNli.OutputScintilla
open Swensen.Unquote
open Swensen.NL.VisualNli
open Xunit

[<Fact>]
let ``ReadBuffer with null arg`` () =
    let rbuff = new ReadBuffer(null)
    test <@ not rbuff.HasNext @>
    test <@ rbuff.Next() = None @>

[<Fact>]
let ``ReadBuffer with no arg`` () =
    let rbuff = new ReadBuffer()
    test <@ not rbuff.HasNext @>
    test <@ rbuff.Next() = None @>

[<Fact>]
let ``ReadBuffer multiple reads to empty buffer`` () =
    let rbuff = new ReadBuffer()
    test <@ not rbuff.HasNext @>
    test <@ rbuff.Next() = None; rbuff.Next() = None; rbuff.Next() = None @>

[<Fact>]
let ``ReadBuffer non empty buffer`` () =
    let rbuff = new ReadBuffer([|1;2|])
    test <@ rbuff.HasNext @>
    test <@ rbuff.Next() = Some(1); rbuff.Next() = Some(2); rbuff.Next() = None @>
    test <@ not rbuff.HasNext @>