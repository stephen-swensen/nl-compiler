module Test.VisualNli.OutputScintilla
open Swensen.Unquote
open Swensen.NL.VisualNli
open Xunit
open System

[<Fact>]
let ``ReadBuffer with null arg`` () =
    let rbuff = new ReadBuffer(null)
    test <@ not rbuff.HasNext @>

[<Fact>]
let ``ReadBuffer with no arg`` () =
    let rbuff = new ReadBuffer()
    test <@ not rbuff.HasNext @>

[<Fact>]
let ``ReadBuffer exn when attempt to read exhausted buffer`` () =
    let rbuff = new ReadBuffer()
    test <@ not rbuff.HasNext @>
    raises<InvalidOperationException> <@ rbuff.Next() @>

[<Fact>]
let ``ReadBuffer non empty buffer`` () =
    let rbuff = new ReadBuffer([|1;2|])
    test <@ rbuff.HasNext @>
    test <@ rbuff.Next() = 1; rbuff.Next() = 2 @>
    test <@ not rbuff.HasNext @>