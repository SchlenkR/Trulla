
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

[
(contexts, List<TypeId ["'T0"]>);
('T0, {rec:TypeId ["'T0"]| collections:Sequence (TypeId ["'T1"]) });
('T0__collections, {rec:TypeId ["'T0"; "collections"]|  });
('T1, {rec:TypeId ["'T1"]| masterData:Sequence (TypeId ["'T2"]) });
('T1__masterData, {rec:TypeId ["'T1"; "masterData"]| orders:Sequence (TypeId ["'T2"]) });
('T1__masterData__orders, {rec:TypeId ["'T1"; "masterData"; "orders"]|  });
('T2, {rec:TypeId ["'T2"]| isDispatched:Mono Bool; number:Mono Str });
('T2__number, {rec:TypeId ["'T2"; "number"]|  });
('T2__isDispatched, {rec:TypeId ["'T2"; "isDispatched"]|  });
(user, {rec:TypeId ["user"]| address:Mono Str });
(user__address, {rec:TypeId ["user"; "address"]| street:Mono Str });
(user__address__street, {rec:TypeId ["user"; "address"; "street"]|  })
]

*)
