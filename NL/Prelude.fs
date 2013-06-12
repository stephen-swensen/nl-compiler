[<AutoOpen>]
module Swensen.NL.Prelude

open System

#nowarn "42" //for raises (inline IL)

///raise is not inlined in Core.Operators, so shows up in stack traces.  We inline it here for clean stacktraces.
let inline raise (e: System.Exception) = (# "throw" e : 'U #)

let time f =
    let sp = System.Diagnostics.Stopwatch.StartNew()
    let r = f()
    sp.Stop()
    Console.WriteLine("time: {0} ", sp.ElapsedMilliseconds)
    Console.Out.Flush()
    r

let (|StringTy|_|) ty =
    if ty = typeof<String> then Some() else None

let (|CharTy|_|) ty =
    if ty = typeof<Char> then Some() else None

let (|ByteTy|_|) ty =
    if ty = typeof<Byte> then Some() else None

let (|SByteTy|_|) ty =
    if ty = typeof<SByte> then Some() else None

let (|UInt16Ty|_|) ty =
    if ty = typeof<UInt16> then Some() else None

let (|UInt32Ty|_|) ty =
    if ty = typeof<UInt32> then Some() else None

let (|UInt64Ty|_|) ty =
    if ty = typeof<UInt64> then Some() else None

let (|Int16Ty|_|) ty =
    if ty = typeof<Int16> then Some() else None

let (|Int32Ty|_|) ty =
    if ty = typeof<Int32> then Some() else None

let (|Int64Ty|_|) ty =
    if ty = typeof<Int64> then Some() else None

let (|BooleanTy|_|) ty =
    if ty = typeof<Boolean> then Some() else None

let (|SingleTy|_|) ty =
    if ty = typeof<Single> then Some() else None

let (|DoubleTy|_|) ty =
    if ty = typeof<Double> then Some() else None

let (|VoidTy|_|) ty =
    if ty = typeof<Void> then Some() else None

let (|EnumTy|_|) (ty:Type) =
    if ty.IsEnum then Some(ty.GetEnumUnderlyingType())
    else None
    
///Used to represent the type of control flow expressions such as break, continue, throw and rethrow
///(these are similar to, but slightly different semantics to void expressions)
type Escape = struct end

let (|EscapeTy|_|) ty =
    if ty = typeof<Escape> then Some() else None

let (|VoidOrEscapeTy|_|) = function
    | VoidTy | EscapeTy -> Some()
    | _ -> None
    
let isVoidOrEscapeTy = function
    | VoidOrEscapeTy -> true
    | _ -> false

let (|ObjEq|ObjNotEq|) (x,y) = 
    if x = y then ObjEq
    else ObjNotEq

let (|Int32|_|) x =
    match Int32.TryParse(x) with
    | true, value -> Some(value)
    | false, _ -> None