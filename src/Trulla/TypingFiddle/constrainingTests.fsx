
#load "typingTestsBase.fsx"

open Trulla.Parsing
open Trulla.Typing
open TypingTestsBase

let x =
    [
        ParserToken.For (pval 0 "matchingContext", accessExp 1 "contexts" [])
        ParserToken.For (pval 2 "customer", accessExp 3 "matchingContext" ["customers"])
        ParserToken.For (pval 4 "order", accessExp 5 "customer" ["activeItems"; "orders"])
        ParserToken.Hole (accessExp 6 "order" ["number"])
        ParserToken.If (accessExp 7 "order" ["isDispatched"])
        ParserToken.End
        ParserToken.Hole (accessExp 8 "user" ["address"; "street"])
        ParserToken.End
        ParserToken.End
        ParserToken.End
    ]
    |> buildTree
    |> buildConstraints 

(*
[
    [contexts] : IsType (Sequence (Ref (TypeId ["'T0"])))
    ['T0] : IsRecord;
    ['T0] : HasField ("customers", Sequence (Ref (TypeId ["'T1"])))
    ['T1; activeItems] : IsRecord;
    ['T1; activeItems] : HasField ("orders", Sequence (Ref (TypeId ["'T2"])))
    ['T2] : IsRecord;
    ['T2] : HasField ("number", Prim Str)
    ['T2] : IsRecord;
    ['T2] : HasField ("isDispatched", Prim Bool)
    [user; address] : IsRecord;
    [user; address] : HasField ("street", Prim Str)
]
*)
