module Tests.PathTests

open Xunit;; open Xunit.Extensions
open Swensen.Unquote.Assertions
open Swensen.NL
open System.Collections.Generic
open System
open Ast
open FSharp.Text.Lexing

open Evaluation

let range (xln,xcol) (yln,ycol) =
    let xpos = { pos_bol = 0
                 pos_fname = ""
                 pos_cnum = xcol
                 pos_lnum = xln
                 //TODO
                 pos_orig_lnum = xln }

    let ypos = { pos_bol = 0
                 pos_fname = ""
                 pos_cnum = ycol
                 pos_lnum = yln
                 //TODO
                 pos_orig_lnum = yln }

    PositionRange(xpos,ypos)

let pos:PositionRange = range (1,0) (1,1)

let xx = "xx", range (1,1) (1,2)
let yy = "yy", range (1,4) (1,5)
let zz = "zz", range (1,7) (1,8)
let multiPartPathSeq = seq { yield xx; yield yy ; yield zz }
let multiPartPath = Path(multiPartPathSeq)
let singlePartPathSeq = multiPartPathSeq |> Seq.take 1
let singlePartPath = Path(singlePartPathSeq)

[<Fact>]
let ``single part equals`` () =
    let s1 = Path(singlePartPathSeq)
    let s2 = Path(singlePartPathSeq)
    test <@ s1.Equals(s2) @>
    test <@ s1 = s2 @>

[<Fact>]
let ``multi part equals`` () =
    let p1 = Path(multiPartPathSeq)
    let p2 = Path(multiPartPathSeq)

    test <@ p1.Equals(p2) @>
    test <@ p1 = p2 @>

[<Fact>]
let ``not equal due to pos`` () =
    let s1 = Path("short", range (1,1) (1,1))
    let s2 = Path("short", range (1,1) (1,5))
    test <@ not <| s1.Equals(s2) @>
    test <@ s1 <> s2 @>

[<Fact>]
let ``not equal due to text`` () =
    let s1 = Path("shortX", range (1,1) (1,5))
    let s2 = Path("short", range (1,1) (1,5))
    test <@ not <| s1.Equals(s2) @>
    test <@ s1 <> s2 @>

[<Fact>]
let ``single part`` () =
    let path = singlePartPath
    test <@ path.Text = "xx" @>
    test <@ path.IsSinglePart = true @>
    test <@ path.LeadingPartsText = "" @>
    test <@ path.LastPartText = "xx" @>
    test <@ path.Expansion |> Seq.toList = [Path([xx]),None] @>
    test <@ path.Pos = range (1,1) (1,2) @>

[<Fact>]
let ``multi part path`` () =
    let path = multiPartPath
    test <@ path.Text = "xx.yy.zz" @>
    test <@ path.IsSinglePart = false @>
    test <@ path.IsMultiPart = true @>
    test <@ path.LeadingPartsText = "xx.yy" @>
    test <@ path.LastPartText = "zz" @>
    test <@ path.Expansion |> Seq.toList = [Path([xx]),Some(Path([yy;zz])); Path([xx;yy]),Some(Path([zz])); path,None] @>
    test <@ path.Pos = range (1,1) (1,8) @>

[<Fact>]
let ``cant be null`` () =
    raises<System.ArgumentNullException> <@ Path(null) @>

[<Fact>]
let ``cant be empty`` () =
    raises<System.ArgumentException> <@ Path([]) @>

[<Fact>]
let ``parts cant contain any null or whitespace parts`` () =
    raises<System.ArgumentException> <@ Path(Seq.append multiPartPathSeq (Seq.singleton ("",pos))) @>


//grammer path cases:
//
//free id (var, static field / property) chain
//id.id.id.id ...
//	-> expr
//	 | expr.id
//	 | expr.id.id
//	 | expr.id.id.id ...
//
//instance calls on an expression (field, property, or method)
//expr.id.id.id ...
//	-> expr.id.id ...
//		-> expr.id ...
//			-> expr ...
//
//
//free id (var, static field / property) chain ending in non-generic method call
//id.id.id.id ... id(args)
//	-> expr.id(args)
//	 | expr.id.id(args)
//	 | expr.id.id.id ... id(args)