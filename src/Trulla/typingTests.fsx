
#r "nuget: FParsec"

#load "parsing.fs"
#load "typing.fs"
open Trulla
open Trulla.Parsing
open Trulla.Typing

let tok number t = 
    let pos = { index = number; line = 0; column = 0 }
    { value = t; start = pos; finish = pos }
let accessExp number ident propPath =
    tok number { ident = ident; propPath = propPath }
let leaf tokenValue = LeafNode tokenValue
let node tokenValue children =
    InternalNode (tokenValue, children)
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()


toTree
    [
        ParserToken.Text "Text1"
        ParserToken.Hole (accessExp 0 "hello" [])
        ParserToken.If (accessExp 1 "cond1" [])
        ParserToken.Text "cond1_Text1"
        ParserToken.Text "cond1_Text2"
        ParserToken.For (tok 2 "x", accessExp 3 "y" [])
        ParserToken.Text "cond1_For1_Text1"
        ParserToken.Text "cond1_For1_Text2"
        ParserToken.End
        ParserToken.Text "cond1_Text3"
        ParserToken.End
        ParserToken.Text "Text2"
    ]
|> shouldEqual
    [
        leaf (Text "Text1")
        leaf (Hole (accessExp 0 "hello" []))
        node (If (accessExp 1 "cond1" [])) [
            leaf (Text "cond1_Text1")
            leaf (Text "cond1_Text2")
            node (For (tok 2 "x", accessExp 3 "y" [])) [
                leaf (Text "cond1_For1_Text1")
                leaf (Text "cond1_For1_Text2")
            ]
            leaf (Text "cond1_Text3")
        ]
        leaf (Text "Text2")
    ]


// Fails because of unclosed scope
toTree [
    ParserToken.If (accessExp 0 "cond1" [])
    ParserToken.Text "Text1"
    ]

// Fails because of unopened scope
toTree [
    ParserToken.If (accessExp 0 "cond1" [])
    ParserToken.Text "Text1"
    ParserToken.End
    ParserToken.End
    ]
