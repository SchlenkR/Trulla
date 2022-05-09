
#r "nuget: FParsec"

#load "parsing.fs"
#load "typing.fs"
open Trulla.Parsing
open Trulla.Typing


let tok number tokenValue =
    let pos = { index = number; line = 0; column = 0 }
    { value = tokenValue; start = pos; finish = pos }
let withTokenPos (tokenValues: TokenValue list) =
    tokenValues |> List.mapi (fun i x -> tok i x)
let node number tokenValue = tok number tokenValue |> Token
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()


toTree (withTokenPos [ 
    Text "Text1"
    PExp(Hole ("hello", []))
    PExp(If ("cond1", []))
    Text "cond1_Text1"
    Text "cond1_Text2"
    PExp(For ("x", ("y",[])))
    Text "cond1_For1_Text1"
    Text "cond1_For1_Text2"
    PExp End
    Text "cond1_Text3"
    PExp End
    Text "Text2"
    ])
|> shouldEqual 
    [
        node 0 (Text "Text1")
        node 1 (PExp (Hole ("hello", [])))
        Scope
            { root = tok 2 (PExp (If ("cond1", [])))
              children = [
                node 3 (Text "cond1_Text1")
                node 4 (Text "cond1_Text2")
                Scope
                    { root = tok 5 (PExp (For ("x", ("y", []))))
                      children = [
                        node 6 (Text "cond1_For1_Text1")
                        node 7 (Text "cond1_For1_Text2")
                        ]
                    }
                node 9 (Text "cond1_Text3")
                ]
            }
        node 11 (Text "Text2")
    ]


// Fails because of unclosed scope
toTree (withTokenPos [
    PExp(If ("cond1", []))
    Text "Text1"
    ])

// Fails because of unopened scope
toTree (withTokenPos [
    PExp(If ("cond1", []))
    Text "Text1"
    PExp End
    PExp End
    ])
