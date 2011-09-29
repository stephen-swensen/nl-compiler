namespace Swensen.NewLang

open System
open Microsoft.FSharp.Text.Lexing

type ErrorMessage = { Message: string ; Code:int }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ErrorMessage = 
    let private usedCodes = ResizeArray.create 0 0 //todo: could not hide this inside mk function for some reason
    ///this function allows us to build a function which produces a ErrorMessage from the format string parameters
    let private mk code (f:Printf.StringFormat<_,ErrorMessage>) =
        //todo: test
        if usedCodes.Contains code then
            raise <| exn(sprintf "Duplicate error code: %i" code)
        else
            usedCodes.Add code

        Printf.ksprintf (fun s -> { Message=s ; Code=code }) f
    
    //error code -1 stands for unspecified error
    
    let Could_not_resolve_type = 
        mk 1 "Could not resolve type: '%s'"
    
    let Could_not_resolve_types = 
        mk 2 "Could not resolve types: '%s'"

    let No_overload_found_for_binary_operator = 
        mk 3 "No overload found for binary operator '%s' with left-hand-side type '%s' and right-hand-side type '%s'"
    
    let Variable_set_type_mismatch = //todo: rewrite to make clearer
        mk 4 "Type mismatch: variable '%s' of type '%s' cannot be assigned a value of the different type '%s'"

    let Variable_not_found = 
        mk 5 "Variable '%s' not found"

    let Expected_type_but_got_type = 
        mk 6 "Expected type '%s' but got type '%s'"

    let Break_outside_of_loop = 
        mk 8 "'break()' is only valid inside a loop body"

    let Continue_outside_of_loop = 
        mk 9 "'continue()' is only valid inside a loop body"

    let Invalid_instance_method = 
        mk 10 "Not a valid instance method '%s' for the given instance type '%s' with argument types %s" //final array of types is sprinted externally

    let Invalid_static_method = 
        mk 11 "Not a valid static method '%s' on the type '%s' with argument types %s" //final array of types is sprinted externally

    let Null_is_invalid_for_value_types = 
        mk 12 "'null' is not a valid value for the value type '%s'"

    let Could_not_resolve_possible_method_call_or_contructor_type = 
        mk 13 "Could not resolve possible method call type '%s' or constructor type '%s'"

    let Void_cannot_be_instantiated = 
        mk 14 "'System.Void' cannot be instantiated"

    let Could_not_resolve_constructor = 
        mk 15 "Could not resolve constructor for type '%s' with argument types %s"

    let Void_invalid_in_let_binding = 
        mk 16 "System.Void is not a valid assignment value in a let binding"

    let Unreachable_code_detected = 
        mk 17 "Unreachable code detected"

    let Namespace_not_found = 
        mk 18 "Namespace '%s' does not exist in any currently open assemblies %s"

    let Could_not_resolve_assembly = 
        mk 19 "Could not resolve assembly reference '%s'"

    let Casting_to_void_invalid = 
        mk 20 "Casting to System.Void will always fail"

    let Casting_noop = 
        mk 21 "Casting a value to its own type '%s' is a no-op"

    let Casting_from_type_to_type_always_invalid =
        mk 22 "Cast from type '%s' to type '%s' will always fail"

    let IfThenElse_branch_type_mismatch =
        mk 23 "'if ... then' and 'else' branches must be of same type but instead are '%s' and '%s'"

        
        
        

type CompilerErrorLevel =
    | Error
    | Warning

type CompilerErrorType =
    | Syntactic
    | Semantic
    | Internal

type PositionRange(posStart:Position, posEnd:Position) =
    member __.Start = posStart
    member __.End = posEnd

type CompilerError(errorRange:PositionRange, errorType:CompilerErrorType, errorLevel:CompilerErrorLevel, errorCode:int, msg:string) =
    member __.Type = errorType
    member __.Range = errorRange
    member __.Level = errorLevel
    member __.Message = msg
    ///The filename corresponding to Range.Start
    member __.Filename = errorRange.Start.FileName
    member __.Code = errorCode
    override this.ToString() =
        let posMsg = 
            if errorRange.Start = errorRange.End then
                sprintf "at line %i, column %i" errorRange.Start.Line errorRange.Start.Column
            else
                sprintf "from line %i, column %i to line %i, column %i"  errorRange.Start.Line errorRange.Start.Column  errorRange.End.Line errorRange.End.Column

        sprintf "%A %s (%i) %s%s%s"
            errorType
            ((sprintf "%A" errorLevel).ToLower())
            errorCode
            posMsg
            (if String.IsNullOrWhiteSpace this.Filename then "" else " in " + this.Filename)
            (if String.IsNullOrWhiteSpace msg then "" else ": " + msg)


type CompilerException(ce: CompilerError) =
    inherit exn(ce.ToString())
    member __.CompilerError = ce

//todo: eventually remove this, we don't want to throw exceptions on error, rather gather as 
//many errors as we can (using error correction) and report back at the end of semantic analysis.
exception SemanticAnalysisException of PositionRange * ErrorMessage