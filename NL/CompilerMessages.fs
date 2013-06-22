namespace Swensen.NL

open System
open System.Diagnostics
open IntelliFactory
open IntelliFactory.Printf

//note performance metrics: semantic analysis on a file with about 8k lines is pretty fast when no errors (~200ms),
//but increases several times when many errors / warnings (about 2k). We were able to cut time by about 50% total with
//the following optimizations: 1) use IntelliFactory.Printf drop-in replacement for F#'s kprintf (shaved off about 8%), 2) do not construct 
//and pass StackTrace into CompilerMessage when not in DEBUG mode (about 40%).

//error messages may be inspired and or copied entirely from C# and F#
///Entry point for all message logging (compiler warnings and errors). Includes canned compiler messages as well as a general
///Log : CompilerMessage -> () function (which should be used sparingly - most compiler messages should be formalized here instead).
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CompilerMessages =
    let inline stacktrace () = 
        #if DEBUG
        StackTrace()
        #else
        null
        #endif

    ///this function allows us to build a function which produces a CompilerMessages from the format string parameters
    let private mk msgLevel msgType code  (pos:PositionRange) (f:Printf.StringFormat<_,unit>) =
        Printf.ksprintf (fun s -> MessageLogger.Log (CompilerMessage(pos, msgType, msgLevel, code, s, stacktrace()))) f
    
    //error code -1 stands for unspecified error

    //let Log msg = MessageLogger.Log(msg)
    
    let Could_not_resolve_type pos = 
        mk MessageLevel.Error MessageType.Semantic 1 pos "Could not resolve type '%s'"
    
    let Could_not_resolve_types pos = 
        mk MessageLevel.Error MessageType.Semantic 2 pos "Could not resolve types: '%s'"

    let No_overload_found_for_binary_operator pos = 
        mk MessageLevel.Error MessageType.Semantic 3 pos "Binary operator '%s' cannot be applied to operands of type '%s' and '%s'"
    
    let Variable_set_type_mismatch pos = //todo: rewrite to make clearer
        mk MessageLevel.Error MessageType.Semantic 4 pos "Type mismatch: variable '%s' of type '%s' cannot be assigned a value of the incompatible type '%s'"

    let Variable_field_or_property_not_found pos = 
        mk MessageLevel.Error MessageType.Semantic 5 pos "Variable, field, or property '%s' not found"

    let Expected_type_but_got_type pos = 
        mk MessageLevel.Error MessageType.Semantic 6 pos "Expected type '%s' but got type '%s'"

    let Break_outside_of_loop pos = 
        mk MessageLevel.Error MessageType.Semantic 8 pos "'break()' is only valid inside a loop body"

    let Continue_outside_of_loop pos = 
        mk MessageLevel.Error MessageType.Semantic 9 pos "'continue()' is only valid inside a loop body"

    let Invalid_instance_method pos = 
        mk MessageLevel.Error MessageType.Semantic 10 pos "Not a valid instance method '%s' for the given instance type '%s' with argument types %s" //final array of types is sprinted externally

    let Invalid_static_method pos = 
        mk MessageLevel.Error MessageType.Semantic 11 pos "Not a valid static method '%s' on the type '%s' with argument types %s" //final array of types is sprinted externally

    let Null_is_invalid_for_value_types pos = 
        mk MessageLevel.Error MessageType.Semantic 12 pos "'null' is not a valid value for the value type '%s'"

    //todo: consider adding "variable not found" possiblity to all of this too, e.g. "x.toString()" trips this error.
    let Could_not_resolve_possible_method_call_or_contructor_type pos = 
        mk MessageLevel.Error MessageType.Semantic 13 pos "Could not resolve possible method call type '%s' or constructor type '%s'"

    let Void_cannot_be_instantiated pos = 
        mk MessageLevel.Error MessageType.Semantic 14 pos "'System.Void' cannot be instantiated"

    let Could_not_resolve_constructor pos = 
        mk MessageLevel.Error MessageType.Semantic 15 pos "Could not resolve constructor for type '%s' with argument types %s"

    let Void_invalid_in_let_binding pos = 
        mk MessageLevel.Error MessageType.Semantic 16 pos "'System.Void' is not a valid assignment value in a let binding"

    let Unreachable_code_detected pos = 
        mk MessageLevel.Warning MessageType.Semantic 17 pos "Unreachable code detected"

    let Namespace_or_type_not_found pos = 
        mk MessageLevel.Error MessageType.Semantic 18 pos "Namespace or type '%s' does not exist in any currently open assemblies %s"

    let Could_not_resolve_assembly pos = 
        mk MessageLevel.Error MessageType.Semantic 19 pos "Could not resolve assembly reference '%s'"

    let Casting_to_void_invalid pos = 
        mk MessageLevel.Error MessageType.Semantic 20 pos "Casting to System.Void will always fail"

    let Casting_noop pos = 
        mk MessageLevel.Error MessageType.Semantic 21 pos "Casting a value to its own type '%s' is a no-op"

    let Casting_from_type_to_type_always_invalid pos =
        mk MessageLevel.Error MessageType.Semantic 22 pos "Cast from type '%s' to type '%s' will always fail"

    let IfThenElse_branch_type_mismatch pos =
        mk MessageLevel.Error MessageType.Semantic 23 pos "'if' and 'else' branches must be of same type but instead are '%s' and '%s'"

    let Internal_error pos =
        mk MessageLevel.Error MessageType.Internal 24 pos "%s"

    let No_overload_found_for_unary_operator pos = 
        mk MessageLevel.Error MessageType.Semantic 25 pos "Unary operator '%s' cannot be applied to operand of type '%s'"

    let Int32_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 26 pos "'System.Int32' literal must be between %i and %i but is %s" Int32.MinValue Int32.MaxValue

    let Double_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 27 pos "'System.Double' literal must be between %f and %f but is %s" Double.MinValue Double.MaxValue

    let Invalid_pathifier pos = 
        mk MessageLevel.Error MessageType.Semantic 28 pos "Path '%s' cannot contain any '%s' characters"

    let Instance_field_or_property_not_found pos = 
        mk MessageLevel.Error MessageType.Semantic 29 pos "Field or property '%s' on instance of type '%s' not found"

    let Field_set_type_mismatch pos =
        mk MessageLevel.Error MessageType.Semantic 30 pos "Type mismatch: field '%s' of type '%s' cannot be assigned a value of the incompatible type '%s'"

    let Property_set_type_mismatch pos =
        mk MessageLevel.Error MessageType.Semantic 31 pos "Type mismatch: property '%s' of type '%s' cannot be assigned a value of the incompatible type '%s'"

    let Property_has_no_setter pos =
        mk MessageLevel.Error MessageType.Semantic 32 pos "The property '%s' has no setter"

    let Property_has_no_getter pos =
        mk MessageLevel.Error MessageType.Semantic 33 pos "The property '%s' has no getter"

    let Could_not_unescape_string_literal pos =
        mk MessageLevel.Error MessageType.Syntactic 34 pos "Error %s"

    let Could_not_unescape_char_literal pos =
        mk MessageLevel.Error MessageType.Syntactic 37 pos "Error %s"

    let Char_literal_must_be_exactly_one_character pos =
        mk MessageLevel.Error MessageType.Semantic 38 pos "Char literal must be exactly one character but was '%s'"

    let Unrecognized_input pos =
        mk MessageLevel.Error MessageType.Syntactic 39 pos "Unrecognized source code character '%s'"

    let SByte_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 40 pos "'System.SByte' literal must be between %i and %i but is %s" SByte.MinValue SByte.MaxValue

    let Byte_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 41 pos "'System.Byte' literal must be between %i and %i but is %s" Byte.MinValue Byte.MaxValue

    let Int16_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 42 pos "'System.Int16' literal must be between %i and %i but is %s" Int16.MinValue Int16.MaxValue

    let UInt16_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 43 pos "'System.UInt16' literal must be between %i and %i but is %s" UInt16.MinValue UInt16.MaxValue

    let UInt32_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 44 pos "'System.UInt32' literal must be between %i and %i but is %s" UInt32.MinValue UInt32.MaxValue

    let Int64_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 45 pos "'System.Int64' literal must be between %i and %i but is %s" Int64.MinValue Int64.MaxValue

    let UInt64_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 46 pos "'System.UInt64' literal must be between %i and %i but is %s" UInt64.MinValue UInt64.MaxValue

    let Single_literal_out_of_range pos = 
        mk MessageLevel.Error MessageType.Semantic 47 pos "'System.Single' literal must be between %f and %f but is %s" Single.MinValue Single.MaxValue

    let Throw_type_does_not_extend_Exception pos =
        mk MessageLevel.Error MessageType.Semantic 48 pos "Cannot throw expression of type '%s' since it does not extend 'System.Exception'"

    let Inconsistent_try_catch_branch_types pos = 
        mk MessageLevel.Error MessageType.Semantic 49 pos "'catch' type '%s' does not match prior 'try' or 'catch' type '%s'"

    let Try_without_catch_or_finally pos = 
        mk MessageLevel.Error MessageType.Semantic 50 pos "'try' is missing required 'catch' or 'finally'"

    let Rethrow_not_valid_outside_of_catch_body pos = 
        mk MessageLevel.Error MessageType.Semantic 51 pos "'rethrow' is not valid outside of a 'catch' body"

    let Rethrow_of_outer_catch_not_valid_inside_nested_finally_body pos = 
        mk MessageLevel.Error MessageType.Semantic 52 pos "'rethrow' of an outer exception is not valid inside a 'finally' body"

    let Parse_error pos =
        mk MessageLevel.Error MessageType.Syntactic 53 pos "Parse error: possible missing or extra syntactic element such as ',' or ';;'"