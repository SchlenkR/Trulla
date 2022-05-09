
#r "nuget: FParsec"

#load "parsing.fs"
#load "typing.fs"
open Trulla.Parsing
open Trulla.Typing

let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()


toTree [ 
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
    ]
|> shouldEqual 
    [
        Token (Text "Text1")
        Token (PExp (Hole ("hello", [])))
        Scope
            { root = If ("cond1", [])
              children = [
                Token (Text "cond1_Text1")
                Token (Text "cond1_Text2")
                Scope
                    { root = For ("x", ("y", []))
                      children = [
                        Token (Text "cond1_For1_Text1")
                        Token (Text "cond1_For1_Text2")
                        ]
                    }
                Token (Text "cond1_Text3")
                ]
            }
        Token (Text "Text2")
    ]


// Fails because of unclosed scope
toTree [
    PExp(If ("cond1", []))
    Text "Text1"
    ]

// Fails because of unopened scope
toTree [
    PExp(If ("cond1", []))
    Text "Text1"
    PExp End
    PExp End
    ]
