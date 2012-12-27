﻿namespace Swensen.NL
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open System.Text.RegularExpressions

type EL = ErrorLogger
module C = Compilation

///The NL interactive
type Nli(?options: CompilerOptions) = 
    let options = defaultArg options CompilerOptions.Default

    let mutable asmCounter = 0I
    let mutable env = options.SemanticEnvironment
    let mutable itCounter = 0I

    //Submit the given NL fragment, returning a list of variables and their values bound to the session.
    member this.TrySubmit(code:string) =
        options.InstallErrorLogger()

        let asmName = "NLI_" + asmCounter.ToString()
        asmCounter <- asmCounter + 1I
        
        let asmFileName = asmName + ".dll"

        let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndSave)
        let modBuilder = asmBuilder.DefineDynamicModule(asmName + ".module", asmFileName, false) //need to specify asmFileName here!
        
        let tyName = asmName + ".TOP_LEVEL"
        let tyBuilder = modBuilder.DefineType(tyName, TypeAttributes.Public)
        
        let tyInitBuilder = tyBuilder.DefineTypeInitializer()

        let ilTopLevel = C.lexParseAndSemantWith env code

        if EL.ActiveLogger.HasErrors then
            None
        else
            let ilTopLevel =
                if options.Optimize then 
                    ilTopLevel |> Optimization.optimize 
                else 
                    ilTopLevel

            match ilTopLevel.NormalizedStmts with
            | None ->
                ErrorMessages.Could_not_normalize_nli_fragment (sprintf "%A" ilTopLevel)
                None
            | Some(stmts) ->
                ///Define the fields to bind to the tyBuilder and define the tyBuilder static constructor which initializes the fields.
                let emit () =
                    let fieldAttrs = FieldAttributes.Public ||| FieldAttributes.Static
                    let il = tyInitBuilder.GetILGenerator()  |> SmartILGenerator.fromILGenerator
                    //need final it
                    for stmt in stmts do
                        match stmt with
                        | ILStmt.Do(x) ->
                            if x.Type <> typeof<Void> then
                                let fi = tyBuilder.DefineField("it" + itCounter.ToString(), x.Type, fieldAttrs)
                                itCounter <- itCounter + 1I
                                Emission.emit il (ILExpr.StaticFieldSet(fi,x))
                            else
                                Emission.emit il x
                        | ILStmt.Let(name,x) -> 
                            let fi = tyBuilder.DefineField(name, x.Type, fieldAttrs)
                            Emission.emit il (ILExpr.StaticFieldSet(fi,x))

                    il.Emit(OpCodes.Ret) //got to remember the static constructor is a method too


                emit ()

                let ty = tyBuilder.CreateType()
                env <- env.ConsType(ty) //cons the new type to the environment so that it is available (and at head of shadowing scope) for the next submit

                //force the ty static constructor to execute (i.e. when we have no fields to init, just code to run)
                //http://stackoverflow.com/a/4181676/236255
                try
                    System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(ty.TypeHandle)
                with 
                    | :? TypeInitializationException as ex when Regex.IsMatch(ex.Message, @"^The type initializer for 'NLI_(\d+).TOP_LEVEL' threw an exception.$") && ex.InnerException <> null ->
                        raise ex.InnerException
                    
                ty.GetFields(BindingFlags.Static ||| BindingFlags.Public)
                |> Seq.map (fun fi -> fi.Name, fi.GetValue(null), fi.FieldType)
                |> Seq.toArray
                |> Some

    member this.Submit(code:string) =
        match this.TrySubmit(code) with
        | Some(xl) -> xl
        | None ->
            raise <| NliException()