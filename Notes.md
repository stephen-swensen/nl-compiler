# About #

I have long been deeply fascinated by programming languages and compilers. My undergraduate degree was in mathematics so although I did take a few computer science courses I never took any compiler courses.

I am a big fan of .NET and F#, the former is a very nice target for a compiler because you get so much for free (a JIT, garbage collector, type system, metadata reader and writer API, ...) and the latter is a language specifically bred for crafting compilers (algebraic data types, pattern matching, HM type inference, ...).

Developing [Unquote](http://code.google.com/p/unquote/) has been great prep experience for understanding ASTs and how to process them. Reading (so far) the first five chapters of _Modern Compiler Implementation in ML_ has been a great theoretical and practical education on lexing, parsing, type checking, and IL emission.

NL comes from "NewLang", since I have so often had some idea for a programming language and quickly created a text document named "NewLang.txt" to jot it down. It also increments 'M' to 'N' from ML, the functional family of languages which F# comes from and which is a sure influence on NL.

# Low-level Design #

Lexing and parsing is done with FsLex and FsYacc. FsYacc is BNF-based parser generator. I really like such parser generators, despite their reputation in some quarters as being old-fashioned, and despite FsYacc lacking virtual any serious tooling and generally not being very high quality (hopefully this will change in the future, and hopefully it will bring EBNF extensions too).

Semantic analysis (type checking) is done using System.Reflection (SR) metadata reading facilities. ~~Currently NL doesn't have any capability to create types, only use those from existing external assemblies, so I'm a little worried about how System.Reflection will hold up in that future.~~ The API itself is fine, but not the friendliest to work with, especially from F# (and my spirit of forgiveness is probably just because I have so much experience with it).

CIL code generation and assembly creation is done using System.Reflection.Emit (SRE), which is just fine, but not great (for example, always having to emit directly to an ILGenerator is not very flexible).

I have considered using [CCI](http://ccimetadata.codeplex.com/) instead of SR and SRE, which looks really nice, but I don't believe it supports generating dynamic assemblies yet, something I wouldn't want to give up since it is the basis of writing a compiler as a service and interactive compiler for NL (F# itself has a translation layer from its internal metadata reader/writer to  System.Reflection.Emit for its interactive compiler, which is an investment)

# High-level Design #

NL is broken up into four main projects
  1. Swensen.NL - the core parsing and compiler facilities exposed as a library ("compiler as a service"). This assembly can be referenced from any .NET project to parse and compiler NL source code at runtime (and do other things to, like hand construct and type check NL ASTs, possibly apply custom transformations for example).
  1. Swensen.NL.Nlc - a console application for compiling NL source files to assemblies on disc.
  1. Swensen.NL.Nli - a console application for interactively compiling bits of NL code. ~~Currently doesn't persist an environment between evaluations.~~
  1. Swensen.NL.VisualNli - a WinForms graphical application for interactively compiling bits of NL code. This project has received a lot more attention than Nlc or Nli, and includes a rich text editor (based on Scintilla) with real-time lexical and semantic analysis.

# Unit Tests #

Compilers are of course excellent candidates for unit testing. NL maintains an exhaustive set of unit tests in the Test.NL project. Testing is done using [Unquote](http://code.google.com/p/unquote/) and [xUnit.net](http://xunit.codeplex.com/). Tests are all currently against Swensen.NL (the core compilation library used by all compilation clients), which means NL fragments can easily be evaluated as strings.

xUnit.net Theories are used to parameterize most test such that assertions are made again both optimized and non-optimized generated IL, thus ensuring equivalence between those compilation options.

Future integration and unit tests are planned for the Swensen.NL.Nli and Swensen.NL.Nlc clients as those components mature.

Code coverage now stands at about 80%.

# Language Design #

The NL language is an evolving design, driven largely by .NET itself (much as C# is), but with syntactic and semantic influences from all the languages I am fond of, especially F#.

Notable features include
  * Everything is an expression, including `open <namespace>` and `ref <assembly>`.
  * Statically type checked; every expression, including null (which is embraced over option types due to .NET's influence for the sake of simplicity)
  * Local type inference
  * Overload resolution
  * Generic types and methods
  * Variables do not need to be explicitly declared, since they are lexically scoped and allow shadowing. A distinction is made between declaring a variable (`<name> = <expression> in <expression>`) and mutating a variable (`<name> <- <expression>`).
  * Coercion of primitive numeric types in numeric expressions
  * Resolves overloaded operators, and implements primitive operators
  * Embraces Void as a valid expression type over a Unit implementation for simplicity, but ensures in semantics analysis that variables for example can't be made to Void expressions.
  * Unified syntax for different box/unbox/conversion/coercion semantics, with semantic checks for detectable invalid compile-time casts
  * while loops with break and continue (yet still an expression)

The [Language Specification](https://docs.google.com/document/d/1bIwwQ2uWBZUIrxxlVCAKPMkMBqbkCLT1VrHY0eC7Nb0/edit?hl=en_US) is a big ongoing effort. The unit tests are well organized and can give a lot of insight into the language syntax and semantics.

# Expectations #

As of now, I don't intend NL to become more than a hobby learning project. I like and am proud of a lot of things about it and am enthused to keep developing it. But it is a first time project on a complex topic, and I think of it mostly as a learning experience.

# Source Control #

This project also represents my first try at using a distributed source control system, Mercurial (Hg), having been converted from an original SVN repository. So don't be surprised if some of my revision history looks a little non-standard at first! I'm also not sure if I will get the full experience of Hg as the only developer (I'll be doing lots of pushes, but not many pulls except perhaps from different locations).

# Error Handling #

We make a full-hearted attempt at implementing solid compiler error handling, including

  * Formal error messages, codes, categorization and position reporting
  * A logging mechanism with suitable abstraction for use in different contexts like command line compiler, compiler as a service, eval compiler, interactive compiler, and VS hosting compiler
  * Sensible error recovery

The list of error messages and error codes can currently be found at http://code.google.com/p/nl-compiler/source/browse/NL/CompilerMessages.fs.

All log messages are sent via the static functions of the `CompilerMessages` module, where `CompilerMessages.Log : CompilerMessage -> ()` is the general entry point for all formalized error messages. `CompilerMessages.Log`s job is to trigger a global, ThreadStatic event which may have zero or more subscribers. `AbstractMessageSink` is an abstract message sink which subscribes to the log event on initialization and unsubscribes when disposed. `BasicMessageSink` is an implementation sufficient for most internal log orchestration (tracking errors through compilation pipeline, optionally logging messages to stdout and stderr). Key takeaways are
  * All messages should be logged via functions of `CompilerMessages`
  * Use subclasses `AbstractMessageSink` to listen to message logging
    * Always dispose of `AbstractMessageSink`s when no longer needed (e.g. via `use sink = ...`)
    * `AbstractMessageSink`s should never span more than one thread (since the event it listens to is implemented as a `ThreadStatic`)
    * `AbstractMessaageSink`s do not interfere with each other: you may safely use zero or more for different purposes as long as they do not span multiple threads as previously mentioned.

# Optimizations #

The NL compiler implements the following optimizations:
  * Constants folding (numeric literal arithmatic and comparison, string literal concat)
  * Dead code trimming