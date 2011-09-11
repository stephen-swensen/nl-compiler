module Tests.DynamicMethodCompilerTests

open Xunit
open Swensen.Unquote
open Swensen.NewLang
module C = Compiler

[<Fact>]
let ``literal System.Int32 exp`` () =
    test <@ C.eval "3" = 3 @>

[<Fact>]
let ``literal System.Double exp`` () =
    test <@ C.eval "3.0" = 3.0 @>

[<Fact>]
let ``literal System.String exp`` () =
    test <@ C.eval "\"hello world\"" = "hello world" @>

[<Fact>]
let ``Int32 addition`` () =
    test <@ C.eval "3 + 3" = 6 @>

[<Fact>]
let ``Double addition`` () =
    test <@ C.eval "3.0 + 3.0" = 6.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in addition`` () =
    test <@ C.eval "3.0 + 3" = 6.0 @>

[<Fact>]
let ``Int32 subtraction`` () =
    test <@ C.eval "3 - 3" = 0 @>

[<Fact>]
let ``Double subtraction`` () =
    test <@ C.eval "3.0 - 3.0" = 0.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in sub`` () =
    test <@ C.eval "3.0 - 3" = 0.0 @>

[<Fact>]
let ``Int32 mult`` () =
    test <@ C.eval "3 * 3" = 9 @>

[<Fact>]
let ``Double mult`` () =
    test <@ C.eval "3.0 * 3.0" = 9.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in mult`` () =
    test <@ C.eval "3.0 * 3" = 9.0 @>

[<Fact>]
let ``Int32 div`` () =
    test <@ C.eval "3 / 3" = 1 @>

[<Fact>]
let ``Double div`` () =
    test <@ C.eval "3.0 / 3.0" = 1.0 @>

[<Fact>]
let ``Lossless coersion of Int32 to Double in div`` () =
    test <@ C.eval "3.0 / 3" = 1.0 @>

[<Fact>]
let ``Pow on floats only`` () =
    test <@ C.eval "3.0 ^ 3.0" = 27.0 @>

[<Fact>]
let ``Factorial on ints only`` () =
    test <@ C.eval "4!" = 24 @>

[<Fact>]
let ``unary minus int`` () =
    test <@ C.eval "-3" = -3 @>

[<Fact>]
let ``unary minus float`` () =
    test <@ C.eval "-3.0" = -3.0 @>

[<Fact>]
let ``* stronger than +`` () =
    test <@ C.eval "2 + 3 * 4" = 14 @>

[<Fact>]
let ``parenthesis to overcome precedence`` () =
    test <@ C.eval "(2 + 3) * 4" = 20 @>

[<Fact>]
let ``String concat`` () =
    test <@ C.eval "\"hello \" + \"world\"" = "hello world" @>

[<Fact>]
let ``var binding to literal value`` () =
    test <@ C.eval "x = 3 in x" = 3 @>

[<Fact>]
let ``var binding to complex expression`` () =
    test <@ C.eval "x = 3 + 3 in x" = 6 @>

[<Fact>]
let ``var binding used in complex body expression`` () =
    test <@ C.eval "x = 3 in x + x + 3" = 9 @>

[<Fact>]
let ``nested var binding`` () =
    test <@ C.eval "x = 3 in y = 5 in x + y" = 8 @>

[<Fact>]
let ``var shadowing`` () =
    test <@ C.eval "x = 3 in y = 5 in x = 5 in x + y" = 10 @>

[<Fact>]
let ``var ids are case insensitive`` () =
    test <@ C.eval "x.x.x = 3 in X.x.X" = 3 @>

[<Fact>]
let ``instance call on value type`` () =
    test <@ C.eval "3.ToString()" = "3" @>

[<Fact>]
let ``instance calls are case insensitive`` () =
    test <@ C.eval "3.tostrINg()" = "3" @>

[<Fact>]
let ``instance call on obj type`` () =
    test <@ C.eval "\"3\".ToString()" = "3" @>

[<Fact>]
let ``static call`` () =
    test <@ C.eval "System.String.Concat(\"hello \", \"world\")" = "hello world" @>

[<Fact>]
let ``static call is case insensitive`` () =
    test <@ C.eval "System.STRING.Concat(\"hello \", \"world\")" = "hello world" @>

[<Fact>]
let ``sequential expression`` () =
    test <@ C.eval "3;4" = 4 @>

[<Fact>]
let ``multiple sequential expressions`` () =
    test <@ C.eval "3;4;5" = 5 @>

[<Fact>]
let ``sequential expressions have weak right associativity`` () =
    test <@ C.eval "2 + 3 ; 3 + 5" = 8 @>

[<Fact>]
let ``sequential expression with rhs void (result does not need to be popped from the stack)`` () =
    test <@ C.eval "system.console.writeline(\"3\"); 4" = 4 @>

[<Fact>]
let ``constructor`` () =
    test <@ C.eval<obj> "system.collections.arraylist()" :? System.Collections.ArrayList @>

[<Fact>]
let ``char literal`` () =
    test <@ C.eval "'c'" = 'c' @>

[<Fact>]
let ``system open by default`` () =
    test <@ C.eval "string('c',3)" = "ccc" @>

[<Fact>]
let ``system.collections open by default`` () =
    test <@ C.eval<obj> "arraylist()" :? System.Collections.ArrayList @>

[<Fact>]
let ``open expression`` () =
    test <@ C.eval<obj> "open System.Diagnostics in Stopwatch()" :? System.Diagnostics.Stopwatch @>

[<Fact>]
let ``ref relative path dll`` () =
    test <@ C.eval<obj> "ref \"xunit.dll\" in Xunit.Record()" :? Xunit.Record @>

[<Fact>]
let ``ref assembly display name`` () =
    test <@ C.eval "ref \"System.Web, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a\" in open System.Web.Mail in SmtpMail.get_SmtpServer()" = "" @>

[<Fact>]
let ``literal true`` () =
    test <@ C.eval "true" = true @>

[<Fact>]
let ``literal false`` () =
    test <@ C.eval "false" = false @>

[<Fact>]
let ``implicit downcast ref type and value type static call args`` () =
    //resolves to String.concat(obj,obj)
    test <@ C.eval "\"asdf\" + 3" = "asdf3" @>

[<Fact>]
let ``null literal`` () =
    test <@ C.eval "null[string]" = (null:string) @>

[<Fact>]
let ``null literal of value type is invalid`` () =
    raises<SemanticErrorException> <@ C.eval "null[int32]" @>

[<Fact>]
let ``+ method overload`` () =
    test <@ C.eval "biginteger(1) + biginteger(2)" = 3I @>

[<Fact>]
let ``- method overload`` () =
    test <@ C.eval "biginteger(1) - biginteger(2)" = -1I @>

[<Fact>]
let ``* method overload`` () =
    test <@ C.eval "biginteger(1) * biginteger(2)" = 2I @>

[<Fact>]
let ``/ method overload`` () =
    test <@ C.eval "biginteger(1) / biginteger(2)" = 0I @>

[<Fact>]
let ``resolve simple fully qualified generic signature in null expression`` () =
    test <@ C.eval "null[system.collections.generic.list[system.int32]]" = null @>

[<Fact>]
let ``resolve simple fully qualified generic signature in constructor`` () =
    test <@ C.eval<obj> "system.collections.generic.list[system.int32]()" :? System.Collections.Generic.List<int> @>

[<Fact>]
let ``resolve simple non qualified generic signature in constructor`` () =
    test <@ C.eval<obj> "list[int32]()" :? System.Collections.Generic.List<int> @>

[<Fact>]
let ``resolve nested non qualified generic signature in constructor`` () =
    test <@ C.eval<obj> "list[list[int32]]()" :? ResizeArray<ResizeArray<int>> @>

open System.Collections.Generic

[<Fact>]
let ``resolve constructor with list of generic args`` () =
    test <@ C.eval<obj> "dictionary[string,string]()" :? Dictionary<string,string> @>

[<Fact>]
let ``resolve complex generic signature in constructor`` () =
    test <@ C.eval<obj> "dictionary[list[int32],dictionary[string,list[int32]]]()" :? Dictionary<ResizeArray<int>,Dictionary<string,ResizeArray<int>>> @>

[<Fact>]
let ``literal typeof value type`` () =
    test <@ C.eval "type[int32]" = typeof<int> @>

[<Fact>]
let ``literal typeof ref type`` () =
    test <@ C.eval "type[string]" = typeof<string> @>

[<Fact>]
let ``literal typeof generic type`` () =
    test <@ C.eval "type[dictionary[string,int32]]" = typeof<Dictionary<string,int>> @>

[<Fact>]
let ``default value of non-primitive value type`` () =
    test <@ C.eval "biginteger()" = bigint() @>

[<Fact>]
let ``default value of bool`` () =
    test <@ C.eval "boolean()" = Unchecked.defaultof<bool> @>

[<Fact>]
let ``default value of int32`` () =
    test <@ C.eval "int32()" = Unchecked.defaultof<int32> @>

[<Fact>]
let ``default value of double`` () =
    test <@ C.eval "double()" = Unchecked.defaultof<double> @>

[<Fact>]
let ``default value of char`` () =
    test <@ C.eval "char()" = Unchecked.defaultof<char> @>

[<Fact>]
let ``call non-generic static method on generic type`` () =
    test <@ C.eval<obj> "HashSet[string].CreateSetComparer()" :? IEqualityComparer<HashSet<string>> @>

[<Fact>]
let ``call generic static method on generic type`` () =
    test <@ C.eval (openPrefix + "Test2[string].DoIt1[int32]()") = 0 @>

[<Fact>]
let ``call generic static method with explicit generic args and no type-wise overloads on non-generic static type`` () = //these could be inferable
    test <@ C.eval "tuple.create[int32,datetime](3, datetime())" = (3, System.DateTime()) @>

[<Fact>]
let ``call generic instance method with explicit generic args and no overloads on var`` () =
    test <@ C.eval (openPrefix + "x = Test1() in x.DoIt2[int32](1)") = 1 @>

[<Fact>]
let ``call generic instance method with explicit generic args and no overloads on expression`` () =
    test <@ C.eval (openPrefix + "Test1().DoIt2[int32](1)") = 1 @>

[<Fact>]
let ``not true`` () =
    test <@ C.eval "~true" = false @>

[<Fact>]
let ``not not true`` () =
    test <@ C.eval "~~true" = true @>

[<Fact>]
let ``not false`` () =
    test <@ C.eval "~false" = true @>

[<Fact>]
let ``not not false`` () =
    test <@ C.eval "~~false" = false @>

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
    raises<SemanticErrorException> <@ C.eval "32[string]" @>

[<Fact>]
let ``semantic error trying to cast sealed ref type to anything other than object or implemented interface `` () =
    raises<SemanticErrorException> <@ C.eval "\"asdf\"[char]" @>

[<Fact>] //fixed
let ``cast var - does not needs to be surrounded with parens`` () =
    test <@ C.eval "x = 3 in x[object][int32]" = 3 @>

[<Fact>]
let ``cast var has more than one generic ty arg`` () =
    raises<SyntaxErrorException> <@ C.eval "x = 3 in x[object,int32]" @>