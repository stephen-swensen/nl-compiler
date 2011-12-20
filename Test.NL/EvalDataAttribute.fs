namespace Tests

open Swensen.NL

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Reflection
open Xunit
open Xunit.Extensions
open System.Reflection

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true, Inherited = true)>]
type EvalDataAttribute() =
    inherit DataAttribute()

    override this.GetData(_:MethodInfo, _:Type[]) : IEnumerable<obj[]> =
        seq {
            yield [| { CompilerOptions.Default with Optimize=false } |]
            yield [| { CompilerOptions.Default with Optimize=true } |]
        }

