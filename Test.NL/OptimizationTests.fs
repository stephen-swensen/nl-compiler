//we strive for 100% code coverage
module Tests.OptimizationTests
open Swensen.NL.Ail

open Xunit
open Swensen.Unquote
open Swensen.NL
open System.Collections.Generic

module O = Optimization
module C = Compilation

//so that we see error details in console output while doing parse operations which don't install their own error loggers
Swensen.NL.ErrorLogger.InstallConsoleLogger()

//N.B. we use parseFromString to obtain a ILExpr tree for convienence and readability, but we are really only testing 
//ILExpr -> ILExpr transformations for optimization. (this does make me a little nervous, of course, but hand constructing
//all these ASTs would be tedious to say the least. we can use code coverage analysis tools like NCover to give us 
//more confidence that we are indeed following all paths).

[<Fact>]
let ``if/then/else unreachable else branch`` () =
    test <@ C.parseFromString "if true { 1 } else { 0 }" |> O.optimize = C.parseFromString "1" @>

[<Fact>]
let ``if/then/else unreachable then branch`` () =
    test <@ C.parseFromString "if false { 1 } else { 0 }" |> O.optimize = C.parseFromString "0" @>

[<Fact>]
let ``if/then/else unreachable else branch condition recursively optimized`` () =
    test <@ C.parseFromString "if if true { true } else { false } { 1 } else { 0 }" |> O.optimize = C.parseFromString "1" @>

[<Fact>]
let ``if/then/else unreachable then branch condition recursively optimized`` () =
    test <@ C.parseFromString "if if false { true } else { false } { 1 } else { 0 }" |> O.optimize = C.parseFromString "0" @>

[<Fact>]
let ``if/then unreachable else branch`` () = //note the expectation of the implicit default[int] in the else branch
    test <@ C.parseFromString "if true { 1;() }" |> O.optimize = C.parseFromString "1;()" @>

[<Fact>]
let ``if/then unreachable then branch`` () =
    test <@ C.parseFromString "if false { 1;() }" |> O.optimize = C.parseFromString "()" @>

[<Fact>]
let ``if/then unreachable else branch condition recursively optimized`` () =
    test <@ C.parseFromString "if (1+1)==2 { () }" |> O.optimize = C.parseFromString "()" @>

[<Fact>]
let ``if/then unreachable then branch condition recursively optimized`` () =
    test <@ C.parseFromString "if (1+1)==3 { () }" |> O.optimize = C.parseFromString "()" @>

[<Fact>]
let ``if/then thenBranch is reduced to nop so just give condition`` () =
    test <@ C.parseFromString "if datetime.isLeapYear(1) { () }" |> O.optimize = C.parseFromString "datetime.isLeapYear(1)" @>

//-----do not need to test && and || optimization since they are implemented in terms of if / then / else

[<Fact>]
let ``String concat folding`` () =
    test <@ C.parseFromString "\"str\" + \"str\"" |> O.optimize = C.parseFromString "\"strstr\"" @>

[<Fact>]
let ``instance call sub expressions optimized`` () =
    test <@ C.parseFromString "(\"str\" + \"str\").EndsWith(\"str\" + \"str\")" |> O.optimize = C.parseFromString "(\"strstr\").EndsWith(\"strstr\")" @>

[<Fact>]
let ``static call sub expressions optimized`` () =
    test <@ C.parseFromString "String.Compare(\"str\" + \"str\", \"str\" + \"str\")" |> O.optimize = C.parseFromString "String.Compare(\"strstr\", \"strstr\")" @>

[<Fact>]
let ``condition is optimized but doesn't result in whole if then else being optimized away`` () =
    test <@ C.parseFromString "if (true || true).getType() == type[boolean] { true } else { false }" |> O.optimize = C.parseFromString "if true.getType() == type[boolean] { true } else { false }" @>

[<Fact>]
let ``Int32 constants folding`` () =
    test <@ C.parseFromString "((2 * 3) + 45) - (24 / 2)" |> O.optimize = C.parseFromString "39" @>

[<Fact>]
let ``Double constants folding`` () =
    test <@ C.parseFromString "((2.0 * 3.0) + 45.0) - (24.0 / 2.0)" |> O.optimize = C.parseFromString "39.0" @>

[<Fact>]
let ``coersion of literal int to double is optimized away`` () =
    test <@ C.parseFromString "2[double]" |> O.optimize = C.parseFromString "2.0" @>

[<Fact>]
let ``coersion of literal double to int is optimized away`` () =
    test <@ C.parseFromString "2.3[int32]" |> O.optimize = C.parseFromString "2" @>

[<Fact>]
let ``coersion subexpression folds`` () =
    test <@ C.parseFromString "(2 + 2)[double]" |> O.optimize = C.parseFromString "4.0" @>

[<Fact>]
let ``coersion subexpression doesn't fold but is optimized`` () =
    test <@ C.parseFromString "x = 1 in (x + (2 + 3))[double]" |> O.optimize = C.parseFromString "x = 1 in (x + 5)[double]" @>

[<Fact>]
let ``constants folding with optimized implicit int to double coersion`` () =
    test <@ C.parseFromString "2 + 3.0" |> O.optimize = C.parseFromString "5.0" @>

[<Fact>]
let ``Int32 equals comparison constants folding true`` () =
    test <@ C.parseFromString "2 == 2" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``Int32 equals comparison constants folding false`` () =
    test <@ C.parseFromString "2 == 3" |> O.optimize = C.parseFromString "false" @>

//don't need to do != this cases since they are logical not (!) optimization

[<Fact>]
let ``Int32 less than comparison constants folding true`` () =
    test <@ C.parseFromString "2 < 3" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``Int32 less than comparison constants folding false`` () =
    test <@ C.parseFromString "3 < 2" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``Int32 greater than comparison constants folding true`` () =
    test <@ C.parseFromString "2 > 1" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``Int32 greater than comparison constants folding false`` () =
    test <@ C.parseFromString "1 > 2" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``logical not of false`` () =
    test <@ C.parseFromString "!false" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``logical not of true`` () =
    test <@ C.parseFromString "!true" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``logical not sub expression folds`` () =
    test <@ C.parseFromString "!(true && true)" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``logical not sub expression doesn't fold but is optimized`` () =
    test <@ C.parseFromString "x = true in !(x && (true && true))" |> O.optimize = C.parseFromString "x = true in !(x && true)" @>

[<Fact>]
let ``comparison op sub expression reduction`` () =
    test <@ C.parseFromString "2 > (1 + 1)" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``numeric binop does not fold but one subexpression does`` () =
    test <@ C.parseFromString "(2 + 1) + (\"asdf\".get_Length())" |> O.optimize = C.parseFromString "3 + (\"asdf\".get_Length())" @>

[<Fact>]
let ``Double equals comparison constants folding true`` () =
    test <@ C.parseFromString "2.0 == 2.0" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``Double equals comparison constants folding false`` () =
    test <@ C.parseFromString "2.0 == 3.0" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``Double less than comparison constants folding true`` () =
    test <@ C.parseFromString "2.0 < 3.0" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``Double less than comparison constants folding false`` () =
    test <@ C.parseFromString "3.0 < 2.0" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``Double greater than comparison constants folding true`` () =
    test <@ C.parseFromString "2.0 > 1.0" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``Double greater than comparison constants folding false`` () =
    test <@ C.parseFromString "1.0 > 2.0" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``Boolean equals comparison constants folding true`` () =
    test <@ C.parseFromString "true == true" |> O.optimize = C.parseFromString "true" @>

[<Fact>]
let ``Boolean equals comparison constants folding false`` () =
    test <@ C.parseFromString "false == true" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``comparison doesn't fold but does optimze subexpression`` () =
    test <@ C.parseFromString "x = 1 in x > (1 + 2)" |> O.optimize = C.parseFromString "x = 1 in x > 3" @>

[<Fact>]
let ``sequence of noops is reduced`` () =
    test <@ C.parseFromString "();();();()" |> O.optimize = C.parseFromString "()" @>

[<Fact>]
let ``sequence of noops and ints ending in int is reduced`` () =
    test <@ C.parseFromString "();1;();1" |> O.optimize = C.parseFromString "1;1" @>

[<Fact>]
let ``sequence of noops and ints ending in noop is reduced`` () =
    test <@ C.parseFromString "();1;();1;()" |> O.optimize = C.parseFromString "1;1;()" @>

[<Fact>]
let ``reduce sequence sub expressions`` () =
    test <@ C.parseFromString "1+2;2+3" |> O.optimize = C.parseFromString "3;5" @>

[<Fact>]
let ``trim explicit noop if then else branch`` () =
    test <@ C.parseFromString "if datetime.isLeapYear(1) { () } else { () }" |> O.optimize = C.parseFromString "if datetime.isLeapYear(1) { () }" @>

[<Fact>]
let ``trim explicit sequence of noop if then else branch`` () =
    test <@ C.parseFromString "if datetime.isLeapYear(1) { () } else { (();();()) }" |> O.optimize = C.parseFromString "if datetime.isLeapYear(1) { () }" @>

[<Fact>]
let ``optimize if/then/else then and else branches`` () =
    test <@ C.parseFromString "if datetime.isLeapYear(1) { 1 + 2 } else { 3 + 4 }" |> O.optimize = C.parseFromString "if datetime.isLeapYear(1) { 3 } else { 7 }" @>

[<Fact>]
let ``optimize if/then then branch`` () =
    test <@ C.parseFromString "if datetime.isLeapYear(1) { 1 + 2; () }" |> O.optimize = C.parseFromString "if datetime.isLeapYear(1) { 3; () }" @>

[<Fact>]
let ``optimize cast subexpression`` () =
    test <@ C.parseFromString "(1 + 1)[object]" |> O.optimize = C.parseFromString "2[object]" @>

[<Fact>]
let ``optimize ctor args`` () =
    test <@ C.parseFromString "biginteger(2 + 1)" |> O.optimize = C.parseFromString "biginteger(3)" @>

[<Fact>]
let ``optimize assignment of var binding`` () =
    test <@ C.parseFromString "x = 1 + 2 in ()" |> O.optimize = C.parseFromString "x = 3 in ()" @>

[<Fact>]
let ``optimize body of var binding`` () =
    test <@ C.parseFromString "x = 1 in 1 + 2" |> O.optimize = C.parseFromString "x = 1 in 3" @>

[<Fact>]
let ``constant fold uminus of int`` () =
    test <@ ILTopLevel.Expr(ILExpr.UMinus(ILExpr.Int32(1), typeof<int32>)) |> O.optimize = ILTopLevel.Expr(ILExpr.Int32(-1)) @>

[<Fact>]
let ``constant fold uminus of double`` () =
    test <@ ILTopLevel.Expr(ILExpr.UMinus(ILExpr.Double(1.), typeof<double>)) |> O.optimize = ILTopLevel.Expr(ILExpr.Double(-1.)) @>

[<Fact>]
let ``uminus no constant fold but sub optimization`` () =
    //need to parenthisize 2 + 3 since addition is left associative and will not fold otheriwise
    test <@ C.parseFromString "x = 1 in -(x + (2 + 3))" |> O.optimize = C.parseFromString "x = 1 in -(x + 5)" @>

[<Fact>]
let ``trim while loop dead code`` () =
    test <@ C.parseFromString "while false { () } " |> O.optimize = C.parseFromString "()" @>

[<Fact>]
let ``while loop condition is optimized`` () =
    test <@ C.parseFromString "x = true in while x && (true && true) { break() }" |> O.optimize = C.parseFromString "x = true in while x && true { break() }" @>

[<Fact>]
let ``while loop body is optimized`` () =
    test <@ C.parseFromString "while true { (true && true); break() }" |> O.optimize = C.parseFromString "while true { true; break() }" @>

[<Fact>]
let ``varset assign is optimized`` () =
    test <@ C.parseFromString "x = 1 in x <- 1 + 1; x" |> O.optimize = C.parseFromString "x = 1 in x <- 2; x" @>

[<Fact>]
let ``can't optimize Error case`` () =
    raises<exn> <@ ILExpr.Error(typeof<System.Boolean>) |> ILTopLevel.Expr |> O.optimize @>

[<Fact>]
let ``default int32 is optimized to its constant`` () =
    test <@ C.parseFromString "default[int32]" |> O.optimize = C.parseFromString "0" @>

[<Fact>]
let ``default double is optimized to its constant`` () =
    test <@ C.parseFromString "default[double]" |> O.optimize = C.parseFromString "0.0" @>

[<Fact>]
let ``default char is optimized to its constant`` () =
    test <@ C.parseFromString "default[char]" |> O.optimize = C.parseFromString "'\000'" @>

[<Fact>]
let ``default bool is optimized to its constant`` () =
    test <@ C.parseFromString "default[boolean]" |> O.optimize = C.parseFromString "false" @>

[<Fact>]
let ``default non primitive is not optimized`` () =
    test <@ let x = C.parseFromString "default[object]" in x |> O.optimize = x @>

[<Fact>]
let ``static field set assign is optimized`` () =
    test <@ C.parseFromString (openPrefix + "NonGenericClass1.static_field_int <- 1 + 1") |> O.optimize = C.parseFromString (openPrefix + "NonGenericClass1.static_field_int <- 2") @>

[<Fact>]
let ``assign of instance field set is optimized`` () =
    test <@ C.parseFromString (openPrefix + "x = NonGenericClass1() in x.instance_field_int <- 1 + 1") |> O.optimize = C.parseFromString (openPrefix + "x = NonGenericClass1() in x.instance_field_int <- 2") @>

[<Fact(Skip="not sure how to find an example to show this")>]
let ``instance of instance field set is optimized`` () =
    ()

[<Fact(Skip="not sure how to find an example to show this")>]
let ``instance of instance field get is optimized`` () =
    ()

[<Fact>] //to at least test path of instance field get even though we can't test the instance actually being optimized
let ``instance of instance field get is valid`` () =
    test <@ C.parseFromString (openPrefix + "x = NonGenericClass1() in x.instance_field_int") |> O.optimize = C.parseFromString (openPrefix + "x = NonGenericClass1() in x.instance_field_int") @>