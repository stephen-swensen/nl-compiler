module Tests.OptimizationTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic

module O = Optimization
module C = Compilation

//N.B. we use parseFromString to obtain a texp tree for convienence and readability, but we are really only testing 
//texp -> texp transformations for optimization. (this does make me a little nervous, of course, but hand constructing
//all these ASTs would be tedious to say the least. we can use code coverage analysis tools like NCover to give us 
//more confidence that we are indeed following all paths).

[<Fact>]
let ``unreachable else branch`` () =
    test <@ C.parseFromString "if true then 1 else 0" |> O.optimize = C.parseFromString "1" @>

[<Fact>]
let ``unreachable then branch`` () =
    test <@ C.parseFromString "if false then 1 else 0" |> O.optimize = C.parseFromString "0" @>

[<Fact>]
let ``unreachable else branch condition recursively optimized`` () =
    test <@ C.parseFromString "if (if true then true else false) then 1 else 0" |> O.optimize = C.parseFromString "1" @>

[<Fact>]
let ``unreachable then branch condition recursively optimized`` () =
    test <@ C.parseFromString "if (if false then true else false) then 1 else 0" |> O.optimize = C.parseFromString "0" @>

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
    test <@ C.parseFromString "if ((true || true).getType() == type[boolean]) then true else false" |> O.optimize = C.parseFromString "if (true.getType() == type[boolean]) then true else false" @>

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
let ``coersion subexpression is optimized`` () =
    test <@ C.parseFromString "(2 + 2)[double]" |> O.optimize = C.parseFromString "4.0" @>

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
let ``logical not sub expression reduction`` () =
    test <@ C.parseFromString "!(true && true)" |> O.optimize = C.parseFromString "false" @>

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