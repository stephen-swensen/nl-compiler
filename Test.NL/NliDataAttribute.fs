namespace Tests

open Swensen.NL

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Reflection
open Xunit
open Xunit.Extensions
open Xunit.Sdk
open System.Reflection

//even though this is currently the same as EvalDataAttribute, we implement as separate attribute
//in order for future flexiblity
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true, Inherited = true)>]
type NliDataAttribute() =
    inherit DataAttribute()

    override this.GetData(_:MethodInfo) : IEnumerable<obj[]> =

        seq {
            yield [| { CompilerOptions.Default with Optimize=false } |]
            yield [| { CompilerOptions.Default with Optimize=true } |]
        }