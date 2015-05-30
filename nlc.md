# About #

nlc.exe is the standalone compiler for NL, a .NET console applications which takes NL source files and produces .NET assemblies


# Compiler Options #

The current usage is very simple:

```

nlc.exe <filename|filename|...> <assembly name>

```

The compiler takes a pipe delimited list of one or file names as the first argument and produces an exe assembly with the name <assembly name>.exe by concatenating all of their text together.

Obviously, much work is to be done.