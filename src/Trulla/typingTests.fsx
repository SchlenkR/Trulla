
#load "typingTestsBase.fsx"

open Trulla.Parsing
open Trulla.Typing
open TypingTestsBase

//let x =
//    toRenderTree [
//        ParserToken.For (pval 0 "matchingContext", accessExp 1 "contexts" [])
//        ParserToken.For (pval 2 "customer", accessExp 3 "matchingContext" ["collections"])
//        ParserToken.For (pval 4 "order", accessExp 5 "customer" ["masterData"; "orders"])
//        ParserToken.Hole (accessExp 6 "order" ["number"])
//        ParserToken.If (accessExp 7 "order" ["isDispatched"])
//        ParserToken.End
//        ParserToken.Hole (accessExp 8 "user" ["address"; "street"])
//        ParserToken.End
//        ParserToken.End
//        ParserToken.End
//    ]
//    |> symbolLinks
//x[range 4]


let x =
    [
        ParserToken.For (pval 0 "matchingContext", accessExp 1 "contexts" [])
        ParserToken.For (pval 2 "customer", accessExp 3 "matchingContext" ["collections"])
        ParserToken.For (pval 4 "order", accessExp 5 "customer" ["masterData"; "orders"])
        ParserToken.Hole (accessExp 6 "order" ["number"])
        ParserToken.If (accessExp 7 "order" ["isDispatched"])
        ParserToken.End
        ParserToken.Hole (accessExp 8 "user" ["address"; "street"])
        ParserToken.End
        ParserToken.End
        ParserToken.End
    ]
    |> toRenderTree
    |> symbolTypes 

//[
//    (["contexts"], Poly (Sequence (Var "'T0")));
//    (["'T0"; "collections"], Poly (Sequence (Var "'T1")));
//    (["'T1"; "masterData"; "orders"], Poly (Sequence (Var "'T2")));
//    (["'T2"; "number"], Mono Str); 
//    (["'T2"; "isDispatched"], Mono Bool);
//    (["user"; "address"; "street"], Mono Str)
//]
