
#r "nuget: FParsec"

#load "parsing.fs"
#load "typing.fs"
open Trulla
open Trulla.Parsing
open Trulla.Typing

let tree number tokenValue =
    let pos = { index = number; line = 0; column = 0 }
    { value = tokenValue; start = pos; finish = pos }
let withTokenPos tokenValues =
    tokenValues |> List.mapi (fun i x -> tree i x)
let leaf number tokenValue =
    LeafNode (tree number tokenValue)
let node number tokenValue children =
    InternalNode (tree number tokenValue, children)
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()


toTree (withTokenPos
    [
        LeafToken (Text "Text1")
        LeafToken (Hole ("hello", []))
        ScopeToken (If ("cond1", []))
        LeafToken (Text "cond1_Text1")
        LeafToken (Text "cond1_Text2")
        ScopeToken (For ("x", ("y",[])))
        LeafToken (Text "cond1_For1_Text1")
        LeafToken (Text "cond1_For1_Text2")
        StupidToken End
        LeafToken (Text "cond1_Text3")
        StupidToken End
        LeafToken (Text "Text2")
    ]
)
|> shouldEqual
    [
        leaf 0 (Text "Text1")
        leaf 1 (Hole ("hello", []))
        node 2 (If ("cond1", [])) [
            leaf 3 (Text "cond1_Text1")
            leaf 4 (Text "cond1_Text2")
            node 5 (For ("x", ("y", []))) [
                leaf 6 (Text "cond1_For1_Text1")
                leaf 7 (Text "cond1_For1_Text2")
            ]
            leaf 9 (Text "cond1_Text3")
        ]
        leaf 11 (Text "Text2")
    ]


// Fails because of unclosed scope
toTree (withTokenPos [
    ScopeToken (If ("cond1", []))
    LeafToken (Text "Text1")
    ])

// Fails because of unopened scope
toTree (withTokenPos [
    ScopeToken (If ("cond1", []))
    LeafToken (Text "Text1")
    StupidToken End
    StupidToken End
    ])
