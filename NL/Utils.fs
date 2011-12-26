namespace Swensen.NL

module Option =
    ///Convert a type which allows null to an option type
    let ofAllowsNull nullable =
        match nullable with
        | null -> None
        | _ -> Some(nullable)

module Seq =
    ///cons x with xs
    let cons x xs =
        seq { yield x ; yield! xs}

module List =
    let contains x = List.exists ((=) x)

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