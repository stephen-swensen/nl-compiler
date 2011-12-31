namespace Swensen.NL

open System

type NumericPrimitive =
    {
        Type:Type
        Implicits: Type list
        Explicits: Type list
    }

//implicits like C# except decimals: http://msdn.microsoft.com/en-us/library/y5b434w4.aspx
//explicits like C# except decimals: http://msdn.microsoft.com/en-us/library/yht2cx7b.aspx
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] 
//should rename to "NumericPrimitive"
module NumericPrimitive =
    let private primitives =
        [
            { 
                Type = typeof<int8>
                Implicits = [typeof<int16>; typeof<int32>; typeof<int64>; typeof<single>; typeof<double>;]
                Explicits = [typeof<uint8>; typeof<uint16>; typeof<uint32>; typeof<uint64>; typeof<char>]
            }
            { 
                Type = typeof<uint8>
                Implicits = [typeof<int16>; typeof<uint16>; typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>; typeof<single>; typeof<double>;]
                Explicits = [typeof<int8>; typeof<char>]
            }
            { 
                Type = typeof<int16>
                Implicits = [typeof<int32>; typeof<int64>; typeof<single>; typeof<double>;]
                Explicits = [typeof<int8>; typeof<uint8>; typeof<uint16>; typeof<uint32>; typeof<uint64>; typeof<char>]
            }
            { 
                Type = typeof<uint16>
                Implicits = [typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>; typeof<single>; typeof<double>;]
                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>; typeof<char>]
            }
            { 
                Type = typeof<int32>
                Implicits = [typeof<int64>; typeof<single>; typeof<double>;]
                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>; typeof<uint16>; typeof<uint32>; typeof<uint64>; typeof<char>]
            }
            { 
                Type = typeof<uint32>
                Implicits = [typeof<int64>; typeof<uint64>; typeof<single>; typeof<double>;]
                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>; typeof<uint16>; typeof<int32>; typeof<char>;]
            }
            { 
                Type = typeof<int64>
                Implicits = [typeof<single>; typeof<double>;]
                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>; typeof<uint16>; typeof<int32>; typeof<uint32>; typeof<uint64>; typeof<char>;]
            }
            { 
                Type = typeof<uint64>
                Implicits = [typeof<single>; typeof<double>;]
                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>; typeof<uint16>; typeof<int32>; typeof<uint32>; typeof<int64>; typeof<char>;]
            }
//            { 
//                Type = typeof<char>
////                Implicits = [typeof<uint16>; typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>; typeof<single>; typeof<double>;]
////                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>;]
//                
//                Implicits = []
//                Explicits = 
//                    [
//                        typeof<int8>; typeof<uint8>; typeof<int16>; //actual implicits
//                        typeof<uint16>; typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>; typeof<single>; typeof<double>; //actual explicits
//                    ]
//            }
            { 
                Type = typeof<single>
                Implicits = [typeof<double>;]
                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>; typeof<uint16>; typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>; typeof<char>;]
            }
            { 
                Type = typeof<double>
                Implicits = []
                Explicits = [typeof<int8>; typeof<uint8>; typeof<int16>; typeof<uint16>; typeof<int32>; typeof<uint32>; typeof<int64>; typeof<uint64>; typeof<char>; typeof<single>;]
            }
        ]

    ///If sourceTy is a primitive and implicitly coercible to targetTy returns true.
    let sourceHasImplicitConvTo sourceTy targetTy =
        primitives 
        |> List.exists (function
            | {Type=ty; Implicits=implicits} when ty = sourceTy ->
                implicits |> List.contains targetTy
            | _ -> false)

    let sourceHasExplicitConvTo sourceTy targetTy =
        primitives 
        |> List.exists (function
            | {Type=ty; Explicits=explicits} when ty = sourceTy ->
                explicits |> List.contains targetTy
            | _ -> false)

    let sourceHasImplicitOrExplicitConvTo sourceTy targetTy =
        sourceHasImplicitConvTo sourceTy targetTy
        || sourceHasExplicitConvTo sourceTy targetTy

    ///If sourceTy is a primitive and equal to or implicitly coercible to targetTy returns true.
    let sourceIsEqualOrHasImplicitConvToTarget sourceTy targetTy =
        primitives 
        |> List.exists (function
            | {Type=ty; Implicits=implicits} when ty = sourceTy ->
                sourceTy = targetTy || implicits |> List.contains targetTy
            | _ -> false)

    ///If ty1 and ty2 are both primitive types then
    ///(if they are equal return Some(their type) else if one can be coerced to the other
    ///return Some(the one that can serve as the coersion target))
    ///else return None
    let tryGetEqualOrImplicitConvTarget ty1 ty2 =
        if sourceIsEqualOrHasImplicitConvToTarget ty1 ty2 then Some(ty2)
        elif sourceIsEqualOrHasImplicitConvToTarget ty2 ty1 then Some(ty1)
        else None

    let isNumericPrimitive ty =
        primitives |> List.exists (function {Type=pty} when pty = ty -> true | _ -> false)