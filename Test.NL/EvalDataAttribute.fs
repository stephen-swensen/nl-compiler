namespace Tests

open Swensen.NL

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Reflection
open Xunit
open Xunit.Extensions
open System.Reflection

/// <summary>
/// Provides a data source for a data theory, with the data coming from a public static property on the test class.
/// The property must return IEnumerable&lt;object[]&gt; with the test data.
/// </summary>
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true, Inherited = true)>]
type EvalDataAttribute() =
    inherit DataAttribute()

    override this.GetData(_:MethodInfo, _:Type[]) : IEnumerable<obj[]> =
        seq {
            yield [| { Compilation.CompilerOptions.Default with Optimize=false } |]
            yield [| { Compilation.CompilerOptions.Default with Optimize=true } |]
        }

