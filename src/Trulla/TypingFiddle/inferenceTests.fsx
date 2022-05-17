
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

let constraints,rangeToTypes = tree |> buildConstraints

let requiredTypes =
    constraints
    |> buildTypes
    |> List.map (fun x -> if x.errors.Length > 0 then failwith "ERROR" else x.typeId,x.resultingTyp)


(*
buildConstraints:

[
    $$ROOT$$ : HasField { name = "contexts" typ = Poly ("sequence", TypeId ["'T0#MATCHINGCONTEXT"]) };
    'T0#MATCHINGCONTEXT : IsRecord;
    'T0#MATCHINGCONTEXT : HasField { name = "customers" typ = Poly ("sequence", TypeId ["'T1#CUSTOMER"]) };
    'T1#CUSTOMER : IsRecord;
    'T1#CUSTOMER : HasField { name = "activeItems" typ = Mono (TypeId ["'T1#CUSTOMER"; "activeItems"]) };
    'T1#CUSTOMER__activeItems : IsRecord;
    'T1#CUSTOMER__activeItems : HasField { name = "orders" typ = Poly ("sequence", TypeId ["'T2#ORDER"]) };
    'T2#ORDER : IsRecord;
    'T2#ORDER : HasField { name = "number" typ = Mono (TypeId ["string"]) };
    'T2#ORDER : IsRecord;
    'T2#ORDER : HasField { name = "isDispatched" typ = Mono (TypeId ["bool"]) };
    $$ROOT$$ : HasField { name = "user" typ = Mono (TypeId ["user"]) }; 
    user : IsRecord;
    user : HasField { name = "address" typ = Mono (TypeId ["user"; "address"]) };
    user__address : IsRecord;
    user__address : HasField { name = "street" typ = Mono (TypeId ["string"]) }
]

*)





(*

requiredTypes


[
    ($$ROOT$$,
        { 
            T4'y: string; 
            T3'x: sequence<T4'y>; 
            user: user; 
            contexts: sequence<T0'matchingContext> 
        });
    (T0'matchingContext, 
        { 
            customers: sequence<T1'customer> 
        });
    (T1'customer, 
        { 
            activeItems: T1'customer__activeItems 
        });
    (T1'customer__activeItems, 
        { 
            orders: sequence<T2'order> 
        });
    (T2'order, 
        { 
            closedItems: sequence<T3'x>; 
            isDispatched: bool; 
            number: string 
        });
    (user, 
        { 
            address: user__address 
        }); 
    (user__address, 
        { 
            street: string 
        })]

*)
