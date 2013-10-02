﻿namespace Tests

open Swensen.NL

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Reflection
open Xunit
open Xunit.Extensions
open System.Reflection

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true, Inherited = true)>]
type AnalysisOnlyDataAttribute() =
    inherit DataAttribute()
    override this.GetData(_:MethodInfo, _:Type[]) : IEnumerable<obj[]> =
        seq {
            yield [| { SemanticEnvironment.Default with IsAnalysisOnly=false } |]
            yield [| { SemanticEnvironment.Default with IsAnalysisOnly=true } |]
        }