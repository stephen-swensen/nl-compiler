namespace Swensen.NL
open Swensen.NL.Ail

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open System.Text.RegularExpressions

type EL = MessageLogger

///The NL interactive
type Nli(?options: CompilerOptions) = 
    let options = defaultArg options CompilerOptions.Default
    let mutable env = options.SemanticEnvironment

    let asmName = "NLI-ASSEMBLY"
    let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(Name=asmName), AssemblyBuilderAccess.RunAndCollect)
    let modBuilder = asmBuilder.DefineDynamicModule(asmName)

    let nextTopLevelTypeName = 
        let count = ref -1
        fun () -> count := !count+1; "TOP_LEVEL" + (!count).ToString()

    let nextItName = 
        let count = ref -1
        fun () -> count := !count+1; "it" + (!count).ToString()

    //Submit the given NL fragment, returning a list of variables and their values bound to the session.
    member this.TrySubmit(code:string, ?offset) =
        let offset = defaultArg offset FrontEnd.DefaultOffset
        use sink = new BasicMessageSink(options.ConsoleLogging)
        
        let ilStmts = FrontEnd.lexParseAndSemantStmtsWith offset env code modBuilder nextTopLevelTypeName nextItName

        if sink.HasErrors then
            None, sink.GetMessages()
        else
            let stmts = if options.Optimize then Optimization.optimizeStmts ilStmts else ilStmts
            if sink.HasErrors then
                None, sink.GetMessages()
            else
                let tys = ilStmts |> List.map (fun ilStmt -> 
                    match ilStmt with
                    | ILStmt.TypeDef(tyBuilder, tyinit) ->
                        let tyInitBuilder = tyBuilder.DefineTypeInitializer()
                        let ilgen = tyInitBuilder.GetILGenerator() |> SmartILGenerator.fromILGenerator
                        let emit = Emission.emit options.Optimize ilgen
                        for tyinitExpr in tyinit do
                            emit tyinitExpr
                        ilgen.Emit(OpCodes.Ret) //got to remember the static constructor is a method too
                        let ty = tyBuilder.CreateType()
                        ty
                    | ILStmt.Error -> 
                        failwith "Should not be emitting opcodes for an ilStmt with errors")
                
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
                            Some(field.Name, field.GetValue(), field.FieldType)
                        | _ -> failwith "Unexpected number of fields in TOP_LEVEL type"
                    ) |> Seq.toList |> Some
                    
                retval, sink.GetMessages()

    member this.Submit(code:string) =
        match this.TrySubmit(code) with
        | Some(xl), _ -> xl
        | None, msgs ->
            raise <| NliException(msgs)