NL is a statically typed programming language targeting .NET.

The compiler is written in F# using FsLex and FsYacc for lexing and parsing, and SRE for CIL emission.

It is a hobby project used by the author as a first exploration of programming language design and compiler implementation.

Links (also see the [Wiki](../../wiki)):

  * [Various notes about NL including design and implementation insight](../../wiki/Notes)
  * [NL Language Specification - Draft](https://docs.google.com/document/d/1bIwwQ2uWBZUIrxxlVCAKPMkMBqbkCLT1VrHY0eC7Nb0/edit?hl=en_US&pli=1)

## Notes on recent upgrade to .NET 7

* .NET 7.0 doesn't support AssemblyBuilder.Save. But that should be coming in .NET 8. 
* There is new position pos_orig_lnum that's part of FsLex that we aren't sure how to use yet.
* VisualNli hasn't been migrated. But it might be possible. But we might prefer to focus energies on VSCode integration.
