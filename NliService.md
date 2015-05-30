# Introduction #

The nli service is a run time service which is part of NL.dll and consumed by both nli.exe and VisualNli.exe and any other .NET assemblies that wish to leverage NL's interactive compilation feature.


# Comparison with F# Interactive (FSI) #

We've recently investigated more deeply how the F# Interactive approaches emitting dynamic assemblies (mostly just exploring the FSI dynamic assembly using FsEye). And we make the following observations:

  1. FSI only emits one assembly for the lifetime of an FSI session (whereas NLI has been emitting an assembly per submission)
  1. FSI emits a separate Type for each interactive statement (currently we attempt to emit one type per interactive statement, but that has been problematic: see [issue 56](https://code.google.com/p/nl-compiler/issues/detail?id=56))
  1. An F# module that binds a variable with the same name twice results in a type where only the last binding with that name is emitted as a property: not sure what happens to the other variable bindings: possibly those are emitted as incremented fields, and only the last binding is referenced via the property. We should keep this in mind when considering possible support for modules in NL (and keep [issue 56](https://code.google.com/p/nl-compiler/issues/detail?id=56) in mind as it relates to multiple bindings of the same name in a series of statements).
  1. We presume that the sole FSI assembly is created with AssemblyBuilderAccess.RunAndCollect, but we haven't proven this yet. We think this is the case because "Resetting' FSI results in a fresh FSI assembly of the same name in the list of assemblies loaded for the current AppDomain: the previous assembly of that name appears nowhere we can find.
  1. Considering the above, we note that currently NL creates its assemblies with AssemblyBuilderAccess.RunAndSave. Such assemblies cannot be unloaded. We should probably make the default RunAndCollect so that they can be properly unloaded when some consuming library or application wishes to "reset" and NL interactive session. At the same time, we should consider supporting RunAndSave as well, so that interactive assemblies may be persisted to disk (see [issue 48](https://code.google.com/p/nl-compiler/issues/detail?id=48)).