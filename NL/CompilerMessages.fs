namespace Swensen.NL

open System
open System.Diagnostics

//note performance metrics: semantic analysis on a file with about 8k lines is pretty fast when no errors (~200ms),
//but increases several times when many errors / warnings (about 2k). We were able to cut time by about 50% total with
//the following optimizations: 1) use String.format instead of kprintf (shaved off about 8%), 2) do not construct 
//and pass StackTrace into CompilerMessage when not in DEBUG mode (about 40%).

//error messages may be inspired and or copied entirely from C# and F#
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CompilerMessages = 
    //n.b. ommitting StackTrace in RELEASE mode shaved off about 40% of time spent 
    let inline stacktrace() =
        #if DEBUG
        StackTrace()
        #else 
        null
        #endif

    ///These functions allows us to build a function which produces a CompilerMessages from the format string parameters
    let private mk0 msgLevel msgType code  (pos:PositionRange) msg =
        MessageLogger.ActiveLogger.Log (CompilerMessage(pos, msgType, msgLevel, code, msg, stacktrace()))

    let private mk1 msgLevel msgType code  (pos:PositionRange) (format:string) =
        (fun (s1:string) -> MessageLogger.ActiveLogger.Log (CompilerMessage(pos, msgType, msgLevel, code, String.Format(format, s1), stacktrace())))

    let private mk2 msgLevel msgType code  (pos:PositionRange) (format:string) =
        (fun (s1:string) (s2:string) -> MessageLogger.ActiveLogger.Log (CompilerMessage(pos, msgType, msgLevel, code, String.Format(format, s1, s2), stacktrace())))

    let private mk3 msgLevel msgType code  (pos:PositionRange) (format:string) =
        (fun (s1:string) (s2:string) (s3:string) -> MessageLogger.ActiveLogger.Log (CompilerMessage(pos, msgType, msgLevel, code, String.Format(format, s1, s2, s3), stacktrace())))

    let private mk4 msgLevel msgType code  (pos:PositionRange) (format:string) =
        (fun (s1:string) (s2:string) (s3:string) (s4:string) -> MessageLogger.ActiveLogger.Log (CompilerMessage(pos, msgType, msgLevel, code, String.Format(format, [|s1; s2; s3; s4|]), stacktrace())))
    
    //error code -1 stands for unspecified error
    
    let Could_not_resolve_type pos = 
        mk1 MessageLevel.Error MessageType.Semantic 1 pos "Could not resolve type '{0}'"
    
    let Could_not_resolve_types pos = 
        mk1 MessageLevel.Error MessageType.Semantic 2 pos "Could not resolve types: '{0}'"

    let No_overload_found_for_binary_operator pos = 
        mk3 MessageLevel.Error MessageType.Semantic 3 pos "Binary operator '{0}' cannot be applied to operands of type '{1}' and '{2}'"
    
    let Variable_set_type_mismatch pos = //todo: rewrite to make clearer
        mk3 MessageLevel.Error MessageType.Semantic 4 pos "Type mismatch: variable '{0}' of type '{1}' cannot be assigned a value of the incompatible type '{2}'"

    let Variable_field_or_property_not_found pos = 
        mk1 MessageLevel.Error MessageType.Semantic 5 pos "Variable, field, or property '{0}' not found"

    let Expected_type_but_got_type pos = 
        mk2 MessageLevel.Error MessageType.Semantic 6 pos "Expected type '{0}' but got type '{1}'"

    let Break_outside_of_loop pos = 
        mk0 MessageLevel.Error MessageType.Semantic 8 pos "'break()' is only valid inside a loop body"

    let Continue_outside_of_loop pos = 
        mk0 MessageLevel.Error MessageType.Semantic 9 pos "'continue()' is only valid inside a loop body"

    let Invalid_instance_method pos = 
        mk3 MessageLevel.Error MessageType.Semantic 10 pos "Not a valid instance method '{0}' for the given instance type '{1}' with argument types {2}" //final array of types is sprinted externally

    let Invalid_static_method pos = 
        mk3 MessageLevel.Error MessageType.Semantic 11 pos "Not a valid static method '{0}' on the type '{1}' with argument types {2}" //final array of types is sprinted externally

    let Null_is_invalid_for_value_types pos = 
        mk1 MessageLevel.Error MessageType.Semantic 12 pos "'null' is not a valid value for the value type '{0}'"

    //todo: consider adding "variable not found" possiblity to all of this too, e.g. "x.toString()" trips this error.
    let Could_not_resolve_possible_method_call_or_contructor_type pos = 
        mk2 MessageLevel.Error MessageType.Semantic 13 pos "Could not resolve possible method call type '{0}' or constructor type '{1}'"

    let Void_cannot_be_instantiated pos = 
        mk0 MessageLevel.Error MessageType.Semantic 14 pos "'System.Void' cannot be instantiated"

    let Could_not_resolve_constructor pos = 
        mk2 MessageLevel.Error MessageType.Semantic 15 pos "Could not resolve constructor for type '{0}' with argument types {1}"

    let Void_invalid_in_let_binding pos = 
        mk0 MessageLevel.Error MessageType.Semantic 16 pos "'System.Void' is not a valid assignment value in a let binding"

    let Unreachable_code_detected pos = 
        mk0 MessageLevel.Warning MessageType.Semantic 17 pos "Unreachable code detected"

    let Namespace_or_type_not_found pos = 
        mk2 MessageLevel.Error MessageType.Semantic 18 pos "Namespace or type '{0}' does not exist in any currently open assemblies {1}"

    let Could_not_resolve_assembly pos = 
        mk1 MessageLevel.Error MessageType.Semantic 19 pos "Could not resolve assembly reference '{0}'"

    let Casting_to_void_invalid pos = 
        mk0 MessageLevel.Error MessageType.Semantic 20 pos "Casting to System.Void will always fail"

    let Casting_noop pos = 
        mk1 MessageLevel.Error MessageType.Semantic 21 pos "Casting a value to its own type '{0}' is a no-op"

    let Casting_from_type_to_type_always_invalid pos =
        mk2 MessageLevel.Error MessageType.Semantic 22 pos "Cast from type '{0}' to type '{1}' will always fail"

    let IfThenElse_branch_type_mismatch pos =
        mk2 MessageLevel.Error MessageType.Semantic 23 pos "'if' and 'else' branches must be of same type but instead are '{0}' and '{1}'"

    let Internal_error pos =
        mk1 MessageLevel.Error MessageType.Internal 24 pos "{0}"

    let No_overload_found_for_unary_operator pos = 
        mk2 MessageLevel.Error MessageType.Semantic 25 pos "Unary operator '{0}' cannot be applied to operand of type '{1}'"

    let Int32_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 26 pos "'System.Int32' literal must be between {0} and {1} but is {2}" (string Int32.MinValue) (string Int32.MaxValue)

    let Double_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 27 pos "'System.Double' literal must be between {0} and {1} but is {2}" (string Double.MinValue) (string Double.MaxValue)

    let Invalid_pathifier pos = 
        mk2 MessageLevel.Error MessageType.Semantic 28 pos "Path '{0}' cannot contain any '{1}' characters"

    let Instance_field_or_property_not_found pos = 
        mk2 MessageLevel.Error MessageType.Semantic 29 pos "Field or property '{0}' on instance of type '{1}' not found"

    let Field_set_type_mismatch pos =
        mk3 MessageLevel.Error MessageType.Semantic 30 pos "Type mismatch: field '{0}' of type '{1}' cannot be assigned a value of the incompatible type '{2}'"

    let Property_set_type_mismatch pos =
        mk3 MessageLevel.Error MessageType.Semantic 31 pos "Type mismatch: property '{0}' of type '{1}' cannot be assigned a value of the incompatible type '{2}'"

    let Property_has_no_setter pos =
        mk1 MessageLevel.Error MessageType.Semantic 32 pos "The property '{0}' has no setter"

    let Property_has_no_getter pos =
        mk1 MessageLevel.Error MessageType.Semantic 33 pos "The property '{0}' has no getter"

    //why is the error msg so non-descript?
    let Could_not_unescape_string_literal pos =
        mk1 MessageLevel.Error MessageType.Syntactic 34 pos "Error {0}"

    let Could_not_normalize_nli_fragment =
        mk1 MessageLevel.Error MessageType.Syntactic 35 PositionRange.Empty "NL fragment could not be normalized for interactive submission: {0}"

    let Could_not_normalize_eval_fragment =
        mk1 MessageLevel.Error MessageType.Syntactic 36 PositionRange.Empty "NL fragment could not be normalized for evaluation: {0}"

    //why is the error msg so non-descript?
    let Could_not_unescape_char_literal pos =
        mk1 MessageLevel.Error MessageType.Syntactic 37 pos "Error {0}"

    let Char_literal_must_be_exactly_one_character pos =
        mk1 MessageLevel.Error MessageType.Semantic 38 pos "Char literal must be exactly one character but was '{0}'"

    let Unrecognized_input pos =
        mk1 MessageLevel.Error MessageType.Syntactic 39 pos "Unrecognized source code character '{0}'"

    let SByte_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 40 pos "'System.SByte' literal must be between {0} and {1} but is {2}" (string SByte.MinValue) (string SByte.MaxValue)

    let Byte_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 41 pos "'System.Byte' literal must be between {0} and {1} but is {2}" (string Byte.MinValue) (string Byte.MaxValue)

    let Int16_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 42 pos "'System.Int16' literal must be between {0} and {1} but is {2}" (string Int16.MinValue) (string Int16.MaxValue)

    let UInt16_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 43 pos "'System.UInt16' literal must be between {0} and {1} but is {2}" (string UInt16.MinValue) (string UInt16.MaxValue)

    let UInt32_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 44 pos "'System.UInt32' literal must be between {0} and {1} but is {2}" (string UInt32.MinValue) (string UInt32.MaxValue)

    let Int64_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 45 pos "'System.Int64' literal must be between {0} and {1} but is {2}" (string Int64.MinValue) (string Int64.MaxValue)

    let UInt64_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 46 pos "'System.UInt64' literal must be between {0} and {1} but is {2}" (string UInt64.MinValue) (string UInt64.MaxValue)

    let Single_literal_out_of_range pos = 
        mk3 MessageLevel.Error MessageType.Semantic 47 pos "'System.Single' literal must be between {0} and {1} but is {2}" (string Single.MinValue) (string Single.MaxValue)

    let Throw_type_does_not_extend_Exception pos =
        mk1 MessageLevel.Error MessageType.Semantic 48 pos "Cannot throw expression of type '{0}' since it does not extend 'System.Exception'"

    let Inconsistent_try_catch_branch_types pos = 
        mk2 MessageLevel.Error MessageType.Semantic 49 pos "'catch' type '{0}' does not match prior 'try' or 'catch' type '{1}'"

    let Try_without_catch_or_finally pos = 
        mk0 MessageLevel.Error MessageType.Semantic 50 pos "'try' is missing required 'catch' or 'finally'"

    let Rethrow_not_valid_outside_of_catch_body pos = 
        mk0 MessageLevel.Error MessageType.Semantic 51 pos "'rethrow' is not valid outside of a 'catch' body"

    let Rethrow_of_outer_catch_not_valid_inside_nested_finally_body pos = 
        mk0 MessageLevel.Error MessageType.Semantic 52 pos "'rethrow' of an outer exception is not valid inside a 'finally' body"