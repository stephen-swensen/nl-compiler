module Tests.LexParseAndSemantTests
open Swensen.NL
open Swensen.Unquote
open Xunit

[<Fact>]
let ``correct pos info spanning lines with default offset`` () =
    let code = """(); 4 * "asdf";;
(); 4 * "asdf";;
(); 4 * "asdf";;"""
    MessageLogger.InstallInMemoryLogger()
    Compilation.lexParseAndSemant code |> ignore
    let msgs = MessageLogger.ActiveLogger.GetMessages()
    let actualMsgs = msgs |> Array.map (fun msg -> msg.ToString())
    //the important part we are looking for is that the reported line and column numbers in the ranges are correct.
    let expectedMsgs = [|"Semantic error (NL0003) from Line 1, Column 5 to Line 1, Column 15: Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from Line 2, Column 5 to Line 2, Column 15: Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from Line 3, Column 5 to Line 3, Column 15: Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    test <@ actualMsgs = expectedMsgs @>


[<Fact>]
let ``correct pos info spanning lines with non-default offset`` () =
    let code = """4 * "asdf";;
(); 4 * "asdf";;"""
    MessageLogger.InstallInMemoryLogger()
    Compilation.lexParseAndSemantWith SemanticEnvironment.Default (2,5,22) code |> ignore
    let msgs = MessageLogger.ActiveLogger.GetMessages()
    let actualMsgs = msgs |> Array.map (fun msg -> msg.ToString())
    //the important part we are looking for is that the reported line and column numbers in the ranges are correct.
    let expectedMsgs = [|"Semantic error (NL0003) from Line 2, Column 5 to Line 2, Column 15: Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from Line 3, Column 5 to Line 3, Column 15: Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    test <@ actualMsgs = expectedMsgs @>