﻿namespace Swensen.NL
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open FSharp.Text.Lexing
open Lexer
open Parser
open System.Text.RegularExpressions

type internal EL = MessageLogger

type NliVariable(fi:FieldInfo) =
    ///The underlying static field info where the nli variable is stored
    member __.FieldInfo = fi
    member __.Name = fi.Name
    ///Get the current value of the nli variable
    member __.Value = fi.GetValue()
    member __.Type = fi.FieldType

type NliSubmitResults = { HasErrors:bool; Variables: NliVariable list; Messages: CompilerMessage[] }

///The NL interactive
type Nli(?options: CompilerOptions) as this =
    let options = defaultArg options CompilerOptions.Default
    let mutable vars = Map.empty : Map<string,NliVariable>

    let asmName = "NLI-ASSEMBLY"
    let asmBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndCollect)
    let modBuilder = asmBuilder.DefineDynamicModule(asmName)
    let mutable env = options.SemanticEnvironment.ConsAssembly(asmBuilder)

    let nextTopLevelTypeName =
        let count = ref -1
        fun () -> count := !count+1; sprintf "TOP_LEVEL%i" !count

    let nextItName =
        let count = ref -1
        fun () -> count := !count+1; sprintf "it%i" !count

    let trySubmit code offset =
        let offset = defaultArg offset FrontEnd.DefaultOffset
        use sink = new BasicMessageSink(options.ConsoleLogging)

        let ilStmts = FrontEnd.lexParseAndSemantStmtsWith offset env code modBuilder nextTopLevelTypeName nextItName

        if sink.HasErrors then
            { HasErrors=true; Variables=[]; Messages=sink.GetMessages()}
        else
            let stmts = if options.Optimize then Optimization.optimizeStmts ilStmts else ilStmts
            if sink.HasErrors then
                { HasErrors=true; Variables=[]; Messages=sink.GetMessages()}
            else
                //generate and collect all types used for top-level bindings
                let tys = ilStmts |> List.map (fun ilStmt ->
                    match ilStmt with
                    | ILStmt.TypeDef(tyBuilder, tyinit, objinit) ->
                        //ty init
                        do
                            let tyInitBuilder = tyBuilder.DefineTypeInitializer()
                            let ilgen = tyInitBuilder.GetILGenerator() |> SmartILGenerator.fromILGenerator
                            let emit = Emission.emit options.Optimize ilgen
                            for tyinitExpr in tyinit do
                                emit tyinitExpr
                            ilgen.Emit(OpCodes.Ret) //got to remember the static constructor is a method too

                        //obj init
                        do
                            let tyCtorBuilder = tyBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
                            let ilgen = tyCtorBuilder.GetILGenerator() |> SmartILGenerator.fromILGenerator
                            let emit = Emission.emit options.Optimize ilgen
                            for objinitExpr in objinit do
                                emit objinitExpr
                            ilgen.Emit(OpCodes.Ret)

                        let ty = tyBuilder.CreateType()
                        ty
                    | ILStmt.Error ->
                        failwith "Should not be emitting opcodes for an ilStmt with errors")

                //run and collect all return values, if any
                let retval =
                    tys
                    |> Seq.filter (fun ty -> ty.FullName.StartsWith("TOP_LEVEL"))
                    |> Seq.choose (fun ty ->
                        //force the ty static constructor to execute (i.e. when we have no fields to init, just code to run)
                        //http://stackoverflow.com/a/4181676/236255
                        try
                            System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(ty.TypeHandle)
                        with
                            | :? TypeInitializationException as ex when Regex.IsMatch(ex.Message, @"^The type initializer for 'TOP_LEVEL(\d+)' threw an exception.$") && ex.InnerException <> null ->
                                raise ex.InnerException
                        let fields = ty.GetFields(BindingFlags.Static ||| BindingFlags.Public)
                        match fields.Length with
                        | 0 -> None
                        | 1 ->
                            env <- env.ConsType(ty)
                            let field = fields.[0]
                            let nv = NliVariable(field)
                            vars <- vars |> Map.add nv.Name nv
                            Some(nv)
                        | _ -> failwith "Unexpected number of fields in TOP_LEVEL type"
                    ) |> Seq.toList

                { HasErrors=false; Variables=retval; Messages=sink.GetMessages()}

    let submit code =
        match trySubmit code None with
        | { HasErrors=false; Variables=vars } -> vars |> List.map (fun v -> v.Name, v.Value, v.Type)
        | { Messages=msgs } ->
            raise <| NliException(msgs)

    let addVar name (o:'a) =
        let code = sprintf "%s = default[%s]" name typeof<'a>.FullName
        let { Variables=[v] } = trySubmit code None
        v.FieldInfo.SetValue(null, o)

    do //add pointer to this in the nli session
        addVar "nli" this

    //Submit the given NL fragment, returning a list of variables and their values bound to the session.
    member this.TrySubmit(code:string, ?offset) =
        trySubmit code offset

    member this.Submit(code:string) =
        submit code

    ///Add a variable to the session (allows you to interact dynamically with static code instances).
    member this.AddVariable(name, o) = addVar name o

    member this.Environment = env
    member this.Options = options
    member this.Variables = vars