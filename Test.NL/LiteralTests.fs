module Tests.LiteralTests

open Xunit;; open Xunit.Extensions

open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open Evaluation

[<Theory;EvalData>]
let ``literal SByte min value`` options =
    test <@ evalWith options "-128y" = -128y @>

[<Theory;EvalData>]
let ``literal SByte neg min value`` options =
    test <@ evalWith options "--128y" = -128y @>

[<Theory;EvalData>]
let ``literal SByte exp`` options =
    test <@ evalWith options "3y" = 3y @>

[<Theory;EvalData>]
let ``literal SByte out too big`` options =
    raisesWith 
        <@ evalWith options "128y" @>
        (expectedErrors [|40|])

[<Theory;EvalData>]
let ``literal SByte too small`` options =
    raisesWith 
        <@ evalWith options "-129y" @>
        (expectedErrors [|40|])


[<Theory;EvalData>]
let ``literal Byte exp`` options =
    test <@ evalWith options "3uy" = 3uy @>

[<Theory;EvalData>]
let ``literal Byte too big`` options =
    raisesWith 
        <@ evalWith options "256uy" @>
        (expectedErrors [|41|])

[<Theory;EvalData>]
let ``literal Byte too small`` options =
    raisesWith 
        <@ evalWith options "-1uy" @>
        (expectedErrors [|41|])

[<Theory;EvalData>]
let ``literal Int16 min value`` options =
    test <@ evalWith options "-32768s" = -32768s @>

[<Theory;EvalData>]
let ``literal Int16 neg min value`` options =
    test <@ evalWith options "--32768s" = -32768s @>

[<Theory;EvalData>]
let ``literal Int16 exp`` options =
    test <@ evalWith options "3s" = 3s @>

[<Theory;EvalData>]
let ``literal Int16 too big`` options =
    raisesWith 
        <@ evalWith options "32768s" @>
        (expectedErrors [|42|])

[<Theory;EvalData>]
let ``literal Int16 too small`` options =
    raisesWith 
        <@ evalWith options "-32770s" @>
        (expectedErrors [|42|])


[<Theory;EvalData>]
let ``literal UInt16 exp`` options =
    test <@ evalWith options "3us" = 3us @>

[<Theory;EvalData>]
let ``literal UInt16 too big`` options =
    raisesWith 
        <@ evalWith options "65536us" @>
        (expectedErrors [|43|])

[<Theory;EvalData>]
let ``literal UInt16 too small`` options =
    raisesWith 
        <@ evalWith options "-1us" @>
        (expectedErrors [|43|])


[<Theory;EvalData>]
let ``literal Int32 min value`` options =
    test <@ evalWith options "-2147483648" = -2147483648 @>

[<Theory;EvalData>]
let ``literal Int32 neg min value`` options =
    test <@ evalWith options "--2147483648" = -2147483648@>

[<Theory;EvalData>]
let ``literal Int32 exp`` options =
    test <@ evalWith options "3" = 3 @>

[<Theory;EvalData>]
let ``literal Int32 too big`` options =
    raisesWith 
        <@ evalWith options "2147483648" @>
        (expectedErrors [|26|])

[<Theory;EvalData>]
let ``literal Int32 too small`` options =
    raisesWith 
        <@ evalWith options "-2147483649" @>
        (expectedErrors [|26|])


[<Theory;EvalData>]
let ``literal UInt32 exp`` options =
    test <@ evalWith options "3u" = 3u @>

[<Theory;EvalData>]
let ``literal UInt32 too big`` options =
    raisesWith 
        <@ evalWith options "4294967296u" @>
        (expectedErrors [|44|])

[<Theory;EvalData>]
let ``literal UInt32 too small`` options =
    raisesWith 
        <@ evalWith options "-1u" @>
        (expectedErrors [|44|])


[<Theory;EvalData>]
let ``literal Int64 min value`` options =
    test <@ evalWith options "-9223372036854775808L" = -9223372036854775808L @>

[<Theory;EvalData>]
let ``literal Int64 neg min value`` options =
    test <@ evalWith options "--9223372036854775808L" = -9223372036854775808L @>

[<Theory;EvalData>]
let ``literal Int64 exp`` options =
    test <@ evalWith options "3L" = 3L @>

[<Theory;EvalData>]
let ``literal Int64 out of range`` options =
    raisesWith 
        <@ evalWith options "9223372036854775808L" @>
        (expectedErrors [|45|])


[<Theory;EvalData>]
let ``literal UInt64 exp`` options =
    test <@ evalWith options "3UL" = 3UL @>

[<Theory;EvalData>]
let ``literal UInt64 too big`` options =
    raisesWith 
        <@ evalWith options "18446744073709551616UL" @>
        (expectedErrors [|46|])

[<Theory;EvalData>]
let ``literal UInt64 too small`` options =
    raisesWith 
        <@ evalWith options "-1UL" @>
        (expectedErrors [|46|])


[<Theory;EvalData>]
let ``literal neg Single exp`` options =
    test <@ evalWith options "-3.0f" = -3.0f @>

[<Theory;EvalData>]
let ``literal Single exp`` options =
    test <@ evalWith options "3.0f" = 3.0f @>

[<Theory;EvalData>]
let ``literal Single out of range`` options =
    raisesWith 
        <@ evalWith options "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999.0f" @>
        (expectedErrors [|47|])


[<Theory;EvalData>]
let ``literal neg Double exp`` options =
    test <@ evalWith options "-3.0" = -3.0 @>

[<Theory;EvalData>]
let ``literal Double exp`` options =
    test <@ evalWith options "3.0" = 3.0 @>

[<Theory;EvalData>]
let ``literal Double out of range`` options =
    raisesWith 
        <@ evalWith options "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999.0" @>
        (expectedErrors [|27|])

module StringLiteralTests =
    [<Theory;EvalData>]
    let ``literal String`` options =
        test <@ evalWith options "\"hello world\"" = "hello world" @>

    [<Theory;EvalData>]
    let ``literal String with escapes`` options =
        test <@ evalWith options @"""\t\n\r\\rhello world""" = "\t\n\r\\rhello world" @>

    [<Theory;EvalData>]
    let ``literal String with quote escape`` options =
        test <@ evalWith options @"""hello \"" world""" = "hello \" world" @>

    [<Theory;EvalData>]
    let ``literal String verbatim`` options =
        test <@ evalWith options @"@""\t\n\r\\rhello world""" = @"\t\n\r\\rhello world" @>

    [<Theory;EvalData>]
    let ``literal String verbatim double quote as quote`` options =
        test <@ evalWith options @"@""hello """" world""" = "hello \" world" @>

    [<Theory;EvalData>]
    let ``literal String verbatim double double quote as double quote`` options =
        test <@ evalWith options @"@""hello """""""" world""" = "hello \"\" world" @>

    [<Theory;EvalData>]
    let ``invalid escape`` options =
        raisesWith 
            <@ evalWith options @"""hello \K world""" @>
            (expectedErrors [|34|])

module CharLiteralTests =
    [<Theory;EvalData>]
    let ``single char literal`` options =
        test <@ evalWith options "'c'" = 'c' @>

    [<Theory;EvalData>]
    let ``char literal single quote`` options =
        raises<CompilerServiceException> <@ evalWith options "'''" @>

    [<Theory;EvalData>]
    let ``single char literal cannot be single backslash`` options =
        raises<CompilerServiceException> <@ evalWith options "'\'" @>

    [<Theory;EvalData>]
    let ``escaped char literal`` options =
        test <@ evalWith options "'\t'" = '\t' @>

    [<Theory;EvalData>]
    let ``escaped escape char`` options =
        test <@ evalWith options @"'\\'" = '\\' @>

    [<Theory;EvalData>]
    let ``escaped char literal more than one char`` options =
        test <@ evalWith options "'\t'" = '\t' @>

    [<Theory;EvalData>]
    let ``not a valid escape sequence`` options =
        raisesWith 
            <@ evalWith options "'\Q'" @>
            (expectedErrors [|37|])

    [<Theory;EvalData>]
    let ``too many chars`` options =
        raisesWith 
            <@ evalWith options "'aaa'" @>
            (expectedErrors [|38|])

    [<Theory;EvalData>]
    let ``too few chars`` options =
        raisesWith 
            <@ evalWith options "''" @>
            (expectedErrors [|38|])

[<Theory;EvalData>]
let ``literal true`` options =
    test <@ evalWith options "true" = true @>

[<Theory;EvalData>]
let ``literal false`` options =
    test <@ evalWith options "false" = false @>

[<Theory;EvalData>]
let ``null literal`` options =
    test <@ evalWith options "null[string]" = (null:string) @>

[<Theory;EvalData>]
let ``null literal of value type is invalid`` options =
    raisesWith
        <@ evalWith options "null[int32]" @>
        (expectedErrors [|12|])

[<Theory;EvalData>]
let ``literal typeof value type`` options =
    test <@ evalWith options "type[int32]" = typeof<int> @>

[<Theory;EvalData>]
let ``literal typeof ref type`` options =
    test <@ evalWith options "type[string]" = typeof<string> @>

[<Theory;EvalData>]
let ``literal typeof generic type`` options =
    test <@ evalWith options "type[dictionary[string,int32]]" = typeof<Dictionary<string,int>> @>

[<Theory;EvalData>]
let ``literal typeof could not resolve type`` options =
    raisesWith 
        <@ evalWith<obj> options "type[INVALID_TYPE]" @>
        (expectedErrors [|1|])

[<Theory;EvalData>]
let ``resolve simple fully qualified generic signature in null expression`` options =
    test <@ evalWith options "null[system.collections.generic.list[system.int32]]" = null @>

[<Theory;EvalData>]
let ``literal nop`` options =
    test <@ evalWith options "()" = null @>

[<Theory;EvalData>]
let ``literal default value type`` options =
    test <@ evalWith options "default[int32]" = 0 @>

[<Theory;EvalData>]
let ``literal default ref type`` options =
    test <@ evalWith options "default[string]" = null @>

[<Theory;EvalData>]
let ``literal default void is invalid`` options =
    raisesWith 
        <@ evalWith options "default[system.void]" @>
        (expectedErrors [|14|])