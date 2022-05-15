﻿
#load "typingTestsBase.fsx"

open Trulla.Parsing
open Trulla.Typing
open TypingTestsBase

let tree =
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
    |> buildTree

let constraints,rangeToTypes = tree |> buildConstraints

let requiredTypes =
    constraints
    |> buildTypes
    |> List.map (fun x -> if x.errors.Length > 0 then failwith "ERROR" else x.typeId,x.resultingTyp)


(*
buildConstraints:

[
    [] : HasField { name = "contexts" typ = Sequence (TypeId ["'T0"]) }; 
    ['T0] : IsRecord;
    ['T0] : HasField { name = "collections" typ = Sequence (TypeId ["'T1"]) };
    ['T1] : IsRecord; 
    ['T1; masterData] : IsRecord;
    ['T1; masterData] : HasField { name = "orders" typ = Sequence (TypeId ["'T2"]) };
    ['T2] : IsRecord; 
    ['T2] : HasField { name = "number" typ = Mono Str };
    ['T2] : IsRecord; 
    ['T2] : HasField { name = "isDispatched" typ = Mono Bool };
    [user] : IsRecord; 
    [user; address] : IsRecord;
    [user; address] : HasField { name = "street" typ = Mono Str }]

*)


(*

[(, { address: Mono (TypeId ["user"; "address"]); 
      isDispatched: Mono (TypeId ["'T2"; "isDispatched"]); 
      number: Mono (TypeId ["'T2"; "number"]); 
      masterData: Mono (TypeId ["'T1"; "masterData"]); 
      collections: Mono (TypeId ["'T0"; "collections"]); 
      contexts:Poly ("sequence", TypeId ["'T0"]) });
 ('T0, {  }); ('T1, { orders:Poly ("sequence", TypeId ["'T2"]) }); ('T2, {  });
 (user, { street:Mono (TypeId ["string"]) })
]

*)