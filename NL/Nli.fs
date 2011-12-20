﻿namespace Swensen.NL
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

type EL = ErrorLogger
open Compilation

///The NL interactive
type Nli(options: CompilerOptions) = 
    let mutable asmCounter = 0I
    let mutable env = options.SemanticEnvironment
    let mutable itCounter = 0I

    new () = Nli(CompilerOptions.Default)

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

        let stmts =
            let ilTopLevel =
                let ilTopLevel = parseAndSemantWith env code
                if options.Optimize then ilTopLevel |> Optimization.optimize else ilTopLevel

            match ilTopLevel with
            | ILTopLevel.StmtList(stmts) -> stmts
            | ILTopLevel.Expr(x) -> [ILStmt.Do(x)]
            | _ -> failwithf "not a valid NLI expression: %A" ilTopLevel //todo: remove
        
        let emit () =
            let fieldAttrs = FieldAttributes.Public ||| FieldAttributes.Static
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


        emit ()

        let ty = tyBuilder.CreateType()
        env <- env.ConsType(ty)

        //force the ty static constructor to execute (i.e. when we have no fields to init, just code to run)
        //http://stackoverflow.com/a/4181676/236255
        System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(ty.TypeHandle)
        
        ty.GetFields(BindingFlags.Static ||| BindingFlags.Public)
        |> Seq.map (fun fi -> fi.Name, fi.GetValue(null))
        |> Seq.toArray