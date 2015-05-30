# About #

VisualNli.exe is the WinForms-based graphical NL interactive compiler.

Code editing and syntax highlighting is done with [SctinillaNET](http://scintillanet.codeplex.com/). Lexical analysis for syntax highlighting is provided by NL's own lexer (we set the Scintilla control's lexer to `Lexer.Container` and hook into the `StyleNeeded` for custom styling).

Bits of NL code are submitted interactively using CTRL+ENTER (if no code selected, then all the code is submitted, otherwise only the selected code is submitted). Resulting values are sent to the Watch tab's customized version of [FsEye](https://code.google.com/p/fseye/) for visual inspection. Additionally stdout and stderr (including compiler error and warning messages) are redirected to the Output tab and stdin is received via a modal dialog prompt.

# Screenshot #

![https://nl-compiler.googlecode.com/hg/images/vnli-screenshot.png](https://nl-compiler.googlecode.com/hg/images/vnli-screenshot.png)

# Performance Metrics of Real-time Error Indicators #

See https://docs.google.com/spreadsheet/ccc?key=0AobG7wyRjKV9dEJTSnJZVWtOOFpkb0tuQkVaZ01peVE&usp=sharing

Files are under under samples dir.

All time measurements in milliseconds.

stats 3 and 4 are best controlled scenarios since they are nearly identical source code except for errors.

## Observations ##

Parse and Semantic Analysis phases are the biggest bottlenecks. Lex and Indicator drawing are near-trivial. Semantic Analysis phase increases significantly when lots of errors. However, we were able to reduce it by about 50% by identifying some bottlenecks.

## Outcomes ##

Semantic analysis on a file with about 8k lines is pretty fast when no errors (~200ms), but increases several times when many errors / warnings (about 2k). We were able to cut time by about 50% total with the following optimizations: 1) use optimized IntelliFactory.Printf drop-in replacement for F#'s notoriously slow kprintf (shaved off about 8%), 2) do not construct and pass StackTrace into CompilerMessage when not in DEBUG mode (about 40%).