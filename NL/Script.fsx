open System

#r @"FSharp.Powerpack"
#r @"C:\Users\Stephen\Documents\Visual Studio 2010\Projects\NL\NL\bin\Release\NL.dll"

open Swensen.NL
module SA = Swensen.NL.SemanticAnalysis


//NOTE WELL "." prefix takes twice as long as ommitting it!
//for _ in {1..10000} do
//    Type.GetType(".System.Collections.Arraylist", false, false) |> ignore

//Type.GetType(".ArrayList", false, false)