module Tests.CastTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
open System.Collections.Generic

module C = Compilation

[<Fact>]
let ``box primitive value type`` () =
    test <@ C.eval "32[object]" = box 32 @>

[<Fact>]
let ``box and ubox value type`` () =
    test <@ C.eval "32[object][int32]" = 32 @>

[<Fact>]
let ``down cast ref type`` () =
    test <@ C.eval "'c'[object]" = box 'c' @>

[<Fact>]
let ``downcast and updown cast ref type`` () =
    test <@ C.eval "'c'[object][char]" = 'c' @>

[<Fact>]
let ``downcast and upcast ref type to and from interface`` () =
    test <@ C.eval<obj> "arraylist()[ienumerable][arraylist]" |> ignore; true @>

[<Fact>]
let ``downcast and upcast value type to and from interface`` () =
    test <@ C.eval<obj> "biginteger()[IComparable][biginteger]" |> ignore; true @>

[<Fact>]
let ``semantic error trying to cast sealed value type to anything other than object or implemented interface `` () =
    raisesWith 
        <@ C.eval "32[string]" @>
        (fun (e:CompilerException) -> <@ e.CompilerError.Type = CompilerErrorType.Semantic @>)

[<Fact>]
let ``semantic error trying to cast sealed ref type to anything other than object or implemented interface `` () =
    raisesWith 
        <@ C.eval "\"asdf\"[char]" @>
        (fun (e:CompilerException) -> <@ e.CompilerError.Type = CompilerErrorType.Semantic @>)

[<Fact>] //fixed
let ``cast var - does not needs to be surrounded with parens`` () =
    test <@ C.eval "x = 3 in x[object][int32]" = 3 @>

[<Fact>]
let ``cast var has more than one generic ty arg`` () =
    raisesWith 
        <@ C.eval "x = 3 in x[object,int32]" @>
        (fun (e:CompilerException) -> <@ e.CompilerError.Type = CompilerErrorType.Syntactic @>)

[<Fact>]
let ``cannot cast value to its own type`` () =
    raisesWith 
        <@ C.eval "3[int32]" @>
        (fun (e:CompilerException) -> <@ e.CompilerError.Type = CompilerErrorType.Semantic @>)

[<Fact>]
let ``cast int to double, a widening coersion`` () =
    test <@ C.eval "3[double]" = 3.0 @>

[<Fact>]
let ``cast double to int, a narrowing coersion`` () =
    test <@ C.eval "3.0[int32]" = 3 @>

[<Fact>]
let ``cast int32 to biginteger, a biginteger.op_implicit `` () =
    test <@ C.eval "3[biginteger]" = 3I @>

[<Fact>]
let ``cast biginteger to int32, a biginteger.op_explicit `` () =
    test <@ C.eval "biginteger()[int32]" = 0 @>

[<Fact>]
let ``casting to Void will always fail`` () =
    raisesWith 
        <@ C.eval "3[object][System.Void]" @>
        (fun (e:CompilerException) -> <@ e.CompilerError.Type = CompilerErrorType.Semantic @>)