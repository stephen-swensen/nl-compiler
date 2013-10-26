namespace Swensen.NL

///this type is opened in the default SemanticEnvironment, making all of its static members available without qualification
///(e.g. within eval and nli, for example)
type NlPrelude =
    static member Eval<'a>(code:string) : 'a =
        Evaluation.eval<'a> code