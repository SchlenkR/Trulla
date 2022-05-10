
#r "nuget: FParsec"

#load "parsing.fs"
#load "typing.fs"
open Trulla
open Trulla.Parsing
open Trulla.Typing

let tree number tokenValue =
    let pos = { index = number; line = 0; column = 0 }
    { value = tokenValue; start = pos; finish = pos }
let withTokenPos (tokenValues: TokenValue list) =
    tokenValues |> List.mapi (fun i x -> tree i x)
let leaf number tokenValue =
    tree number tokenValue |> LeafNode
let node number tokenValue children =
    InternalNode (tree number tokenValue, children)
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()


toTree (withTokenPos
    [
        Parsing.Text "Text1"
        Parsing.Hole ("hello", [])
        Parsing.If ("cond1", [])
        Parsing.Text "cond1_Text1"
        Parsing.Text "cond1_Text2"
        Parsing.For ("x", ("y",[]))
        Parsing.Text "cond1_For1_Text1"
        Parsing.Text "cond1_For1_Text2"
        End
        Parsing.Text "cond1_Text3"
        End
        Parsing.Text "Text2"
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
    Parsing.If ("cond1", [])
    Parsing.Text "Text1"
    ])

// Fails because of unopened scope
toTree (withTokenPos [
    Parsing.If ("cond1", [])
    Parsing.Text "Text1"
    End
    End
    ])
