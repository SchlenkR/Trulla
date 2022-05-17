
#load "typingTestsBase.fsx"

open Trulla.Parsing
open Trulla.Typing
open TypingTestsBase

let tree =
    [
        ParserToken.For (pval 0 "matchingContext", accessExp 1 "contexts" [])
        ParserToken.For (pval 2 "customer", accessExp 3 "matchingContext" ["customers"])
        ParserToken.For (pval 4 "order", accessExp 5 "customer" ["activeItems"; "orders"])
        ParserToken.Hole (accessExp 6 "order" ["number"])
        ParserToken.If (accessExp 7 "order" ["isDispatched"])
        ParserToken.End
        ParserToken.Hole (accessExp 8 "user" ["address"; "street"])
        ParserToken.For (pval 9 "x", accessExp 10 "order" ["closedItems"])
        ParserToken.For (pval 11 "y", accessExp 12 "x" [])
        ParserToken.Hole (accessExp 13 "y" [])
        ParserToken.End
        ParserToken.End
        ParserToken.End
        ParserToken.End
        ParserToken.End
    ]
    |> buildTree

let constraints,rangeToTypes = tree |> collectConstraints

let requiredTypes =
    constraints
    |> unifyConstraints
    |> List.map (fun x -> if x.errors.Length > 0 then failwith "ERROR" else x.typeId,x.resultingTyp)


