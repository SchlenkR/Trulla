
#load "typingTestsBase.fsx"

open Trulla.Parsing
open Trulla.Typing
open TypingTestsBase


tree [
    ParserToken.Text "Text1"
    ParserToken.Hole (accessExp 0 "hello" [])
    ParserToken.If (accessExp 1 "cond1" [])
    ParserToken.Text "cond1_Text1"
    ParserToken.Text "cond1_Text2"
    ParserToken.For (pval 2 "x", accessExp 3 "y" [])
    ParserToken.Text "cond1_For1_Text1"
    ParserToken.Text "cond1_For1_Text2"
    ParserToken.End
    ParserToken.Text "cond1_Text3"
    ParserToken.End
    ParserToken.Text "Text2"
    ]
|> shouldEqual [
    leaf (Text "Text1")
    leaf (Hole (accessExp 0 "hello" []))
    node (If (accessExp 1 "cond1" [])) [
        leaf (Text "cond1_Text1")
        leaf (Text "cond1_Text2")
        node (For (pval 2 "x", accessExp 3 "y" [])) [
            leaf (Text "cond1_For1_Text1")
            leaf (Text "cond1_For1_Text2")
        ]
        leaf (Text "cond1_Text3")
    ]
    leaf (Text "Text2")
]


// Fails because of unclosed scope
tree [
    ParserToken.If (accessExp 0 "cond1" [])
    ParserToken.Text "Text1"
    ]

// Fails because of unopened scope
tree [
    ParserToken.If (accessExp 0 "cond1" [])
    ParserToken.Text "Text1"
    ParserToken.End
    ParserToken.End
    ]
