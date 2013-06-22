module Tests.LexParseAndSemantTests
open Swensen.NL
open Swensen.Unquote
open Xunit

[<Fact>]
let ``correct pos info spanning lines with default offset`` () =
    let code = """(); 4 * "asdf";;
(); 4 * "asdf";;
(); 4 * "asdf";;"""
    use sink = new BasicMessageSink()
    FrontEnd.lexParseAndSemantStmts code |> ignore
    let msgs = sink.GetMessages()
    let actualMsgs = msgs |> Array.map (fun msg -> msg.ToString())
    //the important part we are looking for is that the reported line and column numbers in the ranges are correct.
    let expectedMsgs = [|"Semantic error (NL0003) from (1,5) to (1,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from (2,5) to (2,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from (3,5) to (3,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    test <@ actualMsgs = expectedMsgs @>


[<Fact>]
let ``correct pos info spanning lines with non-default offset`` () =
    let code = """4 * "asdf";;
(); 4 * "asdf";;"""
    use sink = new BasicMessageSink()
    FrontEnd.lexParseAndSemantStmtsWith (2,5,22) SemanticEnvironment.Default code |> ignore
    let msgs = sink.GetMessages()
    let actualMsgs = msgs |> Array.map (fun msg -> msg.ToString())
    //the important part we are looking for is that the reported line and column numbers in the ranges are correct.
    let expectedMsgs = [|"Semantic error (NL0003) from (2,5) to (2,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from (3,5) to (3,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    test <@ actualMsgs = expectedMsgs @>