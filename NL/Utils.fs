namespace Swensen.NL

module Option =
    ///Convert a type which allows null to an option type
    let ofAllowsNull nullable =
        match nullable with
        | null -> None
        | _ -> Some(nullable)

    let getOrDefault fallback input =
        match input with
        | Some(x) -> x 
        | None -> fallback

module Seq =
    ///cons x with xs
    let cons x xs =
        seq { yield x ; yield! xs}

    //copied from fsi:w
    let distinctByResolve primaryKey resolveCollision values = seq { //make lazy seq? (YES, Seq.distinct does)
        let cache = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        for canidateValue in values do
            let key = primaryKey canidateValue 
            match cache.TryGetValue(key) with
            | true, existingValue -> //collision
                match resolveCollision existingValue canidateValue with
                | x when x >= 0 -> () //if existing equal or greater, keep it
                | _ -> //canidate key wins in collision resolution
                    cache.Remove(key) |> ignore
                    cache.Add(key,canidateValue)
            | false, _ ->
                cache.Add(key, canidateValue)

        yield! cache.Values }

module List =
    let contains x = List.exists ((=) x)
    let tryHead xl =
        match xl with
        | [x] -> Some(x) 
        | _ -> None
    let distinctByResolve primaryKey resolveCollision ls =
        match ls with
        | [] | _::[] -> ls //an observable optimization
        | _ ->
            ls
            |> Seq.distinctByResolve primaryKey resolveCollision
            |> List.ofSeq

//    let combine s1 s2 =
//        seq {
//            for x in s1 do
//                for y in s2 do
//                    yield x,y
//        }
//
//    let combine3 s1 s2 s3 =
//        seq {
//            for x in s1 do
//                for y in s2 do
//                    for z in s3 do
//                        yield x,y,z
//        }

open System.Text.RegularExpressions

//copy and pasted from Unquote
///Regex extensions
module Regex =
    type ActiveMatch =
        {
            Match: Match
            MatchValue: string
            Groups: Group list
            OptionalGroups: (Group option) list
            GroupValues: string list
            OptionalGroupValues: (string option) list
        }

    ///<summary>
    ///Test an input string against a regex pattern using the given RegexOptions flags. 
    ///If the match succeeds, returns an ActiveMatch instance, which can be used for further pattern matching.
    ///Note that the implementation takes advantage of the .NET Regex cache.
    ///</summary>
    ///<param name="flags">
    ///The first argument allows you pass in RegexOptions flags. 
    ///</param>
    ///<param name="pattern">
    ///The second argument is the regex pattern. Cannot be null. 
    ///</param>
    ///<param name="input">
    ///The last argument is the input string to test. The input
    ///may be null which would result in a no-match.
    ///</param>
    let (|Match|_|) flags pattern input =
        match input with
        | null -> None //Regex.Match will throw with null input, we return None instead
        | _ ->
            //using the static Regex.Match takes advantage of Regex caching
            match Regex.Match(input, pattern, flags) with
            | m when m.Success -> 
                //n.b. the head value of m.Groups is the match itself, which we discard
                //n.b. if a group is optional and doesn't match, it's Value is ""
                let groups = [for x in m.Groups -> x].Tail
                let optionalGroups = groups |> List.map (fun x -> if x.Success then Some(x) else None)
                let groupValues = groups |> List.map (fun x -> x.Value)
                let optionalGroupValues = optionalGroups |> List.map (function None -> None | Some(x) -> Some(x.Value))

                Some({ Match=m
                       MatchValue=m.Value
                       Groups=groups
                       OptionalGroups=optionalGroups
                       GroupValues=groupValues
                       OptionalGroupValues=optionalGroupValues })
            | _ -> None

    ///Convenience versions of our regex active patterns using RegexOptions.Compiled flag.
    ///If PORTABLE compiler directive defined, then RegexOptions.None flag used.
    module Compiled =
        ///When silverlight mode is None, else is Compiled
        let private compiledRegexOption = 
#if PORTABLE
            RegexOptions.None
#else
            RegexOptions.Compiled
#endif

        let (|Match|_|) = (|Match|_|) compiledRegexOption

    ///Convenience versions of our regex active patterns using RegexOptions.None flag
    module Interpreted =
        let (|Match|_|) = (|Match|_|) RegexOptions.None
