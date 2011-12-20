module Swensen.NL.Compilation
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

type EL = ErrorLogger

type CompilerOptions = { Optimize:bool ; SemanticEnvironment:SemanticEnvironment }
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
module CompilerOptions =
    let Default = { Optimize=true ; SemanticEnvironment=SemanticEnvironment.Default }

let parseAndSemantWith env code =
    let lexbuf = LexBuffer<char>.FromString(code)
    lexbuf.EndPos <- 
        { 
            pos_bol = 0
            pos_fname=""
            pos_cnum=1
            pos_lnum=1 
        }

    try 
        Parser.start Lexer.tokenize lexbuf
        |> SemanticAnalysis.semantWith env

    with
    | CompilerInterruptException ->
        ILTopLevel.Error
    //fslex/yacc do not use specific exception types
    | e when e.Message = "parse error" || e.Message = "unrecognized input" ->
        EL.ActiveLogger.Log
            (CompilerError(PositionRange(lexbuf.StartPos,lexbuf.EndPos), ErrorType.Syntactic, ErrorLevel.Error, -1, e.Message, null)) //todo: we want the real StackTrace
        ILTopLevel.Error
    | e ->
        EL.ActiveLogger.Log
            (CompilerError(PositionRange(lexbuf.StartPos,lexbuf.EndPos), ErrorType.Internal, ErrorLevel.Error, -1, e.ToString(), null))  //todo: we want the real StackTrace
        ILTopLevel.Error

let parseAndSemant = parseAndSemantWith SemanticEnvironment.Default

//should have "tryEval" which doesn't throw?

///Evaluate an NL code string using the default environment.
///If one or more compiler errors occur, then an EvaluationException is throw which contains the list of errors. Warnings are ignored.
let evalWith<'a> options code : 'a = 
    ///Create a dynamic method from a typed expression using the default environment
    let mkDm (ilExpr:ILExpr) =
        let dm = DynamicMethod("Eval", ilExpr.Type, null)
        let il = dm.GetILGenerator()
        Emission.emit il ilExpr
        il.Emit(OpCodes.Ret)
        dm

    EL.InstallErrorLogger() //may want to switch back to previous logger when exiting eval

    let ilTopLevel = parseAndSemantWith options.SemanticEnvironment code 
    match EL.ActiveLogger.ErrorCount with
    | 0 ->
        let ilTopLevel = if options.Optimize then Optimization.optimize ilTopLevel else ilTopLevel
        let ilExpr =
            match ilTopLevel with
            | ILTopLevel.Expr(x) -> x
            | ILTopLevel.StmtList([ILStmt.Do(x)]) -> x
            | _ -> 
                raise (EvaluationException("not a valid eval expression", [||]))

        let dm = mkDm ilExpr
        dm.Invoke(null,null) |> unbox
    | _ ->
        raise (EvaluationException("compiler errors", EL.ActiveLogger.Errors |> Seq.toArray))

let eval<'a> = evalWith<'a> CompilerOptions.Default

//TODO: the following methods are used for nlc.exe and nli.exe. in the future we will have 
//specialized methodss for the Compiler service, vs. nlc.exe, vs. nli.exe with APPRORPIATE ERRORLOGGERS installed.

//todo: currently this function compilies an expression to a single method
//      set as the entry point of an assembly, obviously this needs to evolve.
///ast -> assemblyName -> unit
let compileFromAil ail asmName =
    let asmFileName = asmName + ".exe"
    let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndSave)
    let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".netmodule", asmFileName, false) //need to specify asmFileName here!
    let tyBuilder = modBuilder.DefineType(asmName + ".Application", TypeAttributes.Public)
    let methBuilder = tyBuilder.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Static, typeof<System.Void>, null)
    
    let ilExpr =
        match ail with
        | ILTopLevel.Expr(x) -> x
        | ILTopLevel.StmtList([ILStmt.Do(x)]) -> x
        | _ -> 
            failwithf "not a valid compiler expression: %A" ail //todo: remove

    let il = methBuilder.GetILGenerator()
    Emission.emit il ilExpr
    il.Emit(OpCodes.Ret)

    tyBuilder.CreateType() |> ignore
    asmBuilder.SetEntryPoint(methBuilder, PEFileKinds.ConsoleApplication)
    asmBuilder.Save(asmFileName)

///Compile the given source code string into an assembly
///code -> assemblyName -> unit
let compileFromString = 
    (parseAndSemantWith SemanticEnvironment.Default)>>compileFromAil

///Compile all the given source code files into a single assembly
///fileNames -> assemblyName -> unit
let compileFromFiles fileNames =
    fileNames
    |> Seq.map System.IO.File.ReadAllText
    |> String.concat System.Environment.NewLine
    //|> (fun text -> printfn "%s" text; text)
    |> compileFromString


///The NL interactive
type Nli() = 
    let mutable asmCounter = 0I
    let mutable env = SemanticEnvironment.Default
    let mutable itCounter = 0I

    member this.Submit(code:string) =
        EL.InstallConsoleLogger()

        let asmName = "NLI_" + asmCounter.ToString()
        asmCounter <- asmCounter + 1I
        
        let asmFileName = asmName + ".dll"

        let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndSave)
        let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".module", asmFileName, false) //need to specify asmFileName here!
        
        let tyName = asmName + ".TOP_LEVEL"
        let tyBuilder = modBuilder.DefineType(tyName, TypeAttributes.Public)
        
        let tyInitBuilder = tyBuilder.DefineTypeInitializer()

        let fieldAttrs = FieldAttributes.Public ||| FieldAttributes.Static

        //need to make optimization optional
        match parseAndSemantWith env code with
        | ILTopLevel.StmtList(stmts) -> 
            let il = tyInitBuilder.GetILGenerator()
            //need final it
            for stmt in stmts do
                match stmt with
                | ILStmt.Do(x) ->
                    if x.Type <> typeof<Void> then
                        let fi = tyBuilder.DefineField("it_" + itCounter.ToString(), x.Type, fieldAttrs)
                        itCounter <- itCounter + 1I
                        Emission.emit il (ILExpr.StaticFieldSet(fi,x))
                    else
                        Emission.emit il x
                | ILStmt.Let(name,x) -> 
                    let fi = tyBuilder.DefineField(name, x.Type, fieldAttrs)
                    Emission.emit il (ILExpr.StaticFieldSet(fi,x))

            il.Emit(OpCodes.Ret) //got to remember the static constructor is a method too

        | ilTL ->
            failwithf "not a valid NLI expression: %A" ilTL //todo: remove

        let ty = tyBuilder.CreateType()
        env <- env.ConsType(ty)

        //force the ty static constructor to execute (i.e. when we have no fields to init, just code to run)
        //http://stackoverflow.com/a/4181676/236255
        System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(ty.TypeHandle)
        
        ty.GetFields(BindingFlags.Static ||| BindingFlags.Public)
        |> Seq.map (fun fi -> fi.Name, fi.GetValue(null))
        |> Seq.toArray