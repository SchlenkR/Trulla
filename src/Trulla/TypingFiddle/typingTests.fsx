
#load "typingTestsBase.fsx"

open Trulla.Parsing
open Trulla.Typing
open TypingTestsBase

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
    |> buildTree
    |> buildConstraints 



[
    [contexts] : Sequence (Ref (TypeId ["'T0"]))
    ['T0] : Record
    ['T0] : RecordField ("collections", Sequence (Ref (TypeId ["'T1"])))
    ['T1; masterData] : Record;
    ['T1; masterData] : RecordField ("orders", Sequence (Ref (TypeId ["'T2"])))
    ['T2] : Record;
    ['T2] : RecordField ("number", Prim Str)
    ['T2] : Record;
    ['T2] : RecordField ("isDispatched", Prim Bool)
    [user; address] : Record;
    [user; address] : RecordField ("street", Prim Str)
]

