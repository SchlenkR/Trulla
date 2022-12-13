
#load "typingTestsBase.fsx"

open Trulla.Internal.Parsing
open Trulla.Internal.ModelInference
open TypingTestsBase


buildTree [
    Token.Text "Text1"
    Token.Hole (accessExp 0 "hello" [])
    Token.If (accessExp 1 "cond1" [])
    Token.Text "cond1_Text1"
    Token.Text "cond1_Text2"
    Token.For (pval 2 "x", accessExp 3 "y" [])
    Token.Text "cond1_For1_Text1"
    Token.Text "cond1_For1_Text2"
    Token.End
    Token.Text "cond1_Text3"
    Token.End
    Token.Text "Text2"
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
buildTree [
    Token.If (accessExp 0 "cond1" [])
    Token.Text "Text1"
    ]

// Fails because of unopened scope
buildTree [
    Token.If (accessExp 0 "cond1" [])
    Token.Text "Text1"
    Token.End
    Token.End
    ]
