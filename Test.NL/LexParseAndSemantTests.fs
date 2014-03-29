module Tests.LexParseAndSemantTests
open Swensen.NL
open Swensen.Unquote
open Xunit;; open Xunit.Extensions

let inline testExpectedMessagesWithOffset offset code expectedMsgs env =
    use sink = new BasicMessageSink()
    ignore <| FrontEnd.lexParseAndSemantStmtsWith offset env code (mkTestModBuilder()) nextTopLevelTypeName nextItName
    let msgs = sink.GetMessages()
    let actualMsgs = msgs |> Array.map (fun msg -> msg.ToString())
    test <@ actualMsgs = expectedMsgs @>

let testExpectedMessages = testExpectedMessagesWithOffset FrontEnd.DefaultOffset

[<Theory; AnalysisOnlyData>]
let ``correct pos info spanning lines with default offset`` env =
    let code = """(); 4 * "asdf";;
(); 4 * "asdf";;
(); 4 * "asdf";;"""
    let expectedMsgs = [|"Semantic error (NL0003) from (1,5) to (1,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from (2,5) to (2,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from (3,5) to (3,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    testExpectedMessages code expectedMsgs env

[<Theory; AnalysisOnlyData>]
let ``correct pos info spanning lines with non-default offset`` env =
    let code = """4 * "asdf";;
(); 4 * "asdf";;"""
    let expectedMsgs = [|"Semantic error (NL0003) from (2,5) to (2,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'";
                         "Semantic error (NL0003) from (3,5) to (3,15): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    testExpectedMessagesWithOffset (2,5,22) code expectedMsgs env

[<Theory; AnalysisOnlyData>]
let ``correct pos with single line comment`` env =
    let code = """x = 3 in
//hello world
x * "asdf";;"""
    let expectedMsgs = [|"Semantic error (NL0003) from (3,1) to (3,11): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    testExpectedMessages code expectedMsgs env

[<Theory; AnalysisOnlyData>]
let ``correct pos with inline comment`` env =
    let code = """3 /*he/*asdf*/llo*/ * "asdf";;"""
    let expectedMsgs = [|"Semantic error (NL0003) from (1,1) to (1,29): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    testExpectedMessages code expectedMsgs env

[<Theory; AnalysisOnlyData>]
let ``correct pos with multi line comment`` env =
    let code = """/*
  he/*asdf*/llo
*/ 
3 * "asdf";;"""
    let expectedMsgs = [|"Semantic error (NL0003) from (4,1) to (4,11): Binary operator '*' cannot be applied to operands of type 'Int32' and 'String'"|]
    testExpectedMessages code expectedMsgs env