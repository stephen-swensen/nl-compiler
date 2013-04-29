[<AutoOpen>]
module Swensen.NL.Prelude

open System

#nowarn "42" //for raises (inline IL)

///raise is not inlined in Core.Operators, so shows up in stack traces.  We inline it here for clean stacktraces.
let inline raise (e: System.Exception) = (# "throw" e : 'U #)

let (|StringTy|_|) ty =
    if ty = typeof<string> then Some() else None

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
    