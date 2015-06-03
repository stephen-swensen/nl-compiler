//we strive for 100% code coverage
module Tests.OptimizationTests
open Swensen.NL.Ail

open Xunit
open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic

module O = Optimization
open System

//so that we see error details in console output while doing parse operations which don't install their own error loggers
let consoleSink = new BasicMessageSink(true)

//N.B. we use lexParseAndSemant to obtain a ILExpr tree for convienence and readability, but we are really only testing 
//ILExpr -> ILExpr transformations for optimization. (this does make me a little nervous, of course, but hand constructing
//all these ASTs would be tedious to say the least. we can use code coverage analysis tools like NCover to give us 
//more confidence that we are indeed following all paths).

///used with xunit Theory and PropertyData for some exhaustive repetitive tests on constants folding under operators.
let numericConstantFoldingSuffixes =
    [
        "y"
        "uy"
        "s"
        "us"
        ""
        "u"
        "L"
        "UL"
        ".0f"
        ".0"
    ] |> Seq.map (fun suffix -> [|suffix :> obj|])

let numericDefaultTypeNamesAndSuffixes =
    [
        "sbyte", "y"
        "byte", "uy"
        "int16", "s"
        "uint16", "us"
        "int32", ""
        "uint32", "u"
        "int64", "L"
        "uint64", "UL"
        "single", ".0f"
        "double", ".0"
    ] |> Seq.map (fun (tyName, suffix) -> [|tyName :> obj; suffix :> obj|])



[<Fact>]
let ``if/then/else unreachable else branch`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if true { 1 } else { 0 }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "1" @>

[<Fact>]
let ``if/then/else unreachable then branch`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if false { 1 } else { 0 }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "0" @>

[<Fact>]
let ``if/then/else unreachable else branch condition recursively optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if if true { true } else { false } { 1 } else { 0 }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "1" @>

[<Fact>]
let ``if/then/else unreachable then branch condition recursively optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if if false { true } else { false } { 1 } else { 0 }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "0" @>

[<Fact>]
let ``if/then unreachable else branch`` () = //note the expectation of the implicit default[int] in the else branch
    test <@ FrontEnd.lexParseAndSemantExpr "if true { 1;() }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "1;()" @>

[<Fact>]
let ``if/then unreachable then branch`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if false { 1;() }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "()" @>

[<Fact>]
let ``if/then unreachable else branch condition recursively optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if (1+1)==2 { () }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "()" @>

[<Fact>]
let ``if/then unreachable then branch condition recursively optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if (1+1)==3 { () }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "()" @>

[<Fact>]
let ``if/then thenBranch is reduced to nop so just give condition`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { () }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "datetime.isLeapYear(1)" @>

//-----do not need to test && and || optimization since they are implemented in terms of if / then / else

[<Fact>]
let ``String concat folding`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "\"str\" + \"str\"" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "\"strstr\"" @>

[<Fact>]
let ``instance call sub expressions optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "(\"str\" + \"str\").EndsWith(\"str\" + \"str\")" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "(\"strstr\").EndsWith(\"strstr\")" @>

[<Fact>]
let ``static call sub expressions optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "String.Compare(\"str\" + \"str\", \"str\" + \"str\")" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "String.Compare(\"strstr\", \"strstr\")" @>

[<Fact>]
let ``condition is optimized but doesn't result in whole if then else being optimized away`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if (true || true).getType() == type[boolean] { true } else { false }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "if true.getType() == type[boolean] { true } else { false }" @>

[<Theory;PropertyData("numericConstantFoldingSuffixes")>]
let ``numeric literal constants folding`` (suffix:string) =
    let input = System.String.Format("((2{0} * 3{0}) + 45{0}) - (24{0} / 2{0})", suffix)
    let outcome = sprintf "39%s" suffix
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr outcome @>

[<Fact>]
let ``coersion of literal int to double is optimized away`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "2[double]" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "2.0" @>

[<Fact>]
let ``coersion of literal double to int is optimized away`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "2.3[int32]" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "2" @>

[<Fact>]
let ``coersion subexpression folds`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "(2 + 2)[double]" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "4.0" @>

[<Fact>]
let ``coersion subexpression doesn't fold but is optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "x = 1 in (x + (2 + 3))[double]" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = 1 in (x + 5)[double]" @>

[<Fact>]
let ``constants folding with optimized implicit int to double coersion`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "2 + 3.0" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "5.0" @>

[<Theory;PropertyData("numericConstantFoldingSuffixes")>]
let ``numeric literal equals comparison constants folding true`` (suffix:string) =
    let input = String.Format("2{0} == 2{0}", suffix)
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "true" @>

[<Theory;PropertyData("numericConstantFoldingSuffixes")>]
let ``numeric literal equals comparison constants folding false`` (suffix:string) =
    let input = String.Format("2{0} == 3{0}", suffix)
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

//don't need to do != this cases since they are logical not (!) optimization

[<Theory;PropertyData("numericConstantFoldingSuffixes")>]
let ``numeric literal less than comparison constants folding true`` (suffix:string) =
    let input = String.Format("2{0} < 3{0}", suffix)
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "true" @>

[<Theory;PropertyData("numericConstantFoldingSuffixes")>]
let ``numeric literal less than comparison constants folding false`` (suffix:string) =
    let input = String.Format("3{0} < 2{0}", suffix)
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

[<Theory;PropertyData("numericConstantFoldingSuffixes")>]
let ``numeric literal greater than comparison constants folding true`` (suffix:string) =
    let input = String.Format("2{0} > 1{0}", suffix)
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "true" @>

[<Theory;PropertyData("numericConstantFoldingSuffixes")>]
let ``numeric literal greater than comparison constants folding false`` (suffix:string) =
    let input = String.Format("1{0} > 2{0}", suffix)
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

[<Fact>]
let ``logical not of false`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "!false" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "true" @>

[<Fact>]
let ``logical not of true`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "!true" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

[<Fact>]
let ``logical not sub expression folds`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "!(true && true)" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

[<Fact>]
let ``logical not sub expression doesn't fold but is optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "x = true in !(x && (true && true))" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = true in !(x && true)" @>

[<Fact>]
let ``comparison op sub expression reduction`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "2 > (1 + 1)" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

[<Fact>]
let ``numeric binop does not fold but one subexpression does`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "(2 + 1) + (\"asdf\".get_Length())" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "3 + (\"asdf\".get_Length())" @>

[<Fact>]
let ``Boolean equals comparison constants folding true`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "true == true" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "true" @>

[<Fact>]
let ``Boolean equals comparison constants folding false`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "false == true" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

[<Fact>]
let ``comparison doesn't fold but does optimze subexpression`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "x = 1 in x > (1 + 2)" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = 1 in x > 3" @>

[<Fact>]
let ``sequence of noops is reduced`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "();();();()" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "()" @>

[<Fact>]
let ``sequence of noops and ints ending in int is reduced`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "();1;();1" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "1;1" @>

[<Fact>]
let ``sequence of noops and ints ending in noop is reduced`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "();1;();1;()" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "1;1;()" @>

[<Fact>]
let ``reduce sequence sub expressions`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "1+2;2+3" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "3;5" @>

[<Fact>]
let ``trim explicit noop if then else branch`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { () } else { () }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { () }" @>

[<Fact>]
let ``trim explicit sequence of noop if then else branch`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { () } else { (();();()) }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { () }" @>

[<Fact>]
let ``optimize if/then/else then and else branches`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { 1 + 2 } else { 3 + 4 }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { 3 } else { 7 }" @>

[<Fact>]
let ``optimize if/then then branch`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { 1 + 2; () }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "if datetime.isLeapYear(1) { 3; () }" @>

[<Fact>]
let ``optimize cast subexpression`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "(1 + 1)[object]" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "2[object]" @>

[<Fact>]
let ``optimize ctor args`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "biginteger(2 + 1)" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "biginteger(3)" @>

[<Fact>]
let ``optimize assignment of var binding`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "x = 1 + 2 in ()" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = 3 in ()" @>

[<Fact>]
let ``optimize body of var binding`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "x = 1 in 1 + 2" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = 1 in 3" @>

///todo: constants folding for other uminus numeric literals
[<Fact>]
let ``constant fold uminus of sbyte`` () =
    test <@ ILExpr.UMinus(false, ILExpr.SByte(1y), typeof<int32>) |> O.optimizeExpr = ILExpr.SByte(-1y) @>

[<Fact>]
let ``constant fold uminus of int16`` () =
    test <@ (ILExpr.UMinus(false, ILExpr.Int16(1s), typeof<int32>)) |> O.optimizeExpr = (ILExpr.Int16(-1s)) @>

[<Fact>]
let ``constant fold uminus of int32`` () =
    test <@ (ILExpr.UMinus(false, ILExpr.Int32(1), typeof<int32>)) |> O.optimizeExpr = (ILExpr.Int32(-1)) @>

[<Fact>]
let ``constant fold uminus of int64`` () =
    test <@ (ILExpr.UMinus(false, ILExpr.Int64(1L), typeof<int32>)) |> O.optimizeExpr = (ILExpr.Int64(-1L)) @>

[<Fact>]
let ``constant fold uminus of single`` () =
    test <@ (ILExpr.UMinus(false, ILExpr.Single(1.f), typeof<single>)) |> O.optimizeExpr = (ILExpr.Single(-1.f)) @>

[<Fact>]
let ``constant fold uminus of double`` () =
    test <@ (ILExpr.UMinus(false, ILExpr.Double(1.), typeof<double>)) |> O.optimizeExpr = (ILExpr.Double(-1.)) @>

//[<Fact>]
//let ``constant fold uminus of char`` () =
//    test <@ (ILExpr.UMinus(false, ILExpr.Char('a'), typeof<char>)) |> O.optimizeExpr = (ILExpr.Char(-('a'|>int)|>char)) @>

[<Fact>]
let ``uminus no constant fold but sub optimization`` () =
    //need to parenthisize 2 + 3 since addition is left associative and will not fold otheriwise
    test <@ FrontEnd.lexParseAndSemantExpr "x = 1 in -(x + (2 + 3))" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = 1 in -(x + 5)" @>

[<Fact>]
let ``trim while loop dead code`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "while false { () } " |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "()" @>

[<Fact>]
let ``while loop condition is optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "x = true in while x && (true && true) { break() }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = true in while x && true { break() }" @>

[<Fact>]
let ``while loop body is optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "while true { (true && true); break() }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "while true { true; break() }" @>

[<Fact>]
let ``varset assign is optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "x = 1 in x <- 1 + 1; x" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "x = 1 in x <- 2; x" @>

[<Fact>]
let ``can't optimize Error case`` () =
    raises<exn> <@ ILExpr.Error(typeof<System.Boolean>) |> O.optimizeExpr @>

[<Theory;PropertyData("numericDefaultTypeNamesAndSuffixes")>]
let ``default numeric primitive is optimized to its constant`` (tyName:string) (suffix:string) =
    let input = sprintf "default[%s]" tyName
    let expected = sprintf "0%s" suffix
    test <@ FrontEnd.lexParseAndSemantExpr input |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr expected @>

[<Fact>]
let ``default char is optimized to its constant`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "default[char]" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "'\000'" @>

[<Fact>]
let ``default bool is optimized to its constant`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "default[boolean]" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr "false" @>

[<Fact>]
let ``default non primitive is not optimized`` () =
    test <@ let x = FrontEnd.lexParseAndSemantExpr "default[object]" in x |> O.optimizeExpr = x @>

[<Fact>]
let ``static field set assign is optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr (openPrefix + "NonGenericClass1.static_field_int <- 1 + 1") |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr (openPrefix + "NonGenericClass1.static_field_int <- 2") @>

[<Fact>]
let ``assign of instance field set is optimized`` () =
    test <@ FrontEnd.lexParseAndSemantExpr (openPrefix + "x = NonGenericClass1() in x.instance_field_int <- 1 + 1") |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr (openPrefix + "x = NonGenericClass1() in x.instance_field_int <- 2") @>

[<Fact(Skip="not sure how to find an example to show this")>]
let ``instance of instance field set is optimized`` () =
    ()

[<Fact(Skip="not sure how to find an example to show this")>]
let ``instance of instance field get is optimized`` () =
    ()

[<Fact>] //to at least test path of instance field get even though we can't test the instance actually being optimized
let ``instance of instance field get is valid`` () =
    test <@ FrontEnd.lexParseAndSemantExpr (openPrefix + "x = NonGenericClass1() in x.instance_field_int") |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr (openPrefix + "x = NonGenericClass1() in x.instance_field_int") @>

[<Fact>]
let ``optimize throw expression`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "throw(exception((1 + 1).tostring()))" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr ("throw(exception(2.tostring()))") @>

[<Fact>]
let ``catches following filterless catch are trimmed`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "try { 1 } catch { 2 } catch[exception] x { 3 }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr ("try { 1 } catch { 2 }") @>

[<Fact>]
let ``unit finally is trimmed`` () =
    test <@ FrontEnd.lexParseAndSemantExpr "try { 1 } catch { 2 } finally { ();() }" |> O.optimizeExpr = FrontEnd.lexParseAndSemantExpr ("try { 1 } catch { 2 }") @>