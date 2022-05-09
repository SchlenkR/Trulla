
#r "nuget: FParsec"
open FParsec

#load "parsing.fs"
#load "typing.fs"
open Trulla.Parsing
open Trulla.Typing


let test p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg
let tok number tokenValue =
    let pos = { index = number; line = 0; column = 0 }
    { value = tokenValue; start = pos; finish = pos }
let setTokenPos (tokenValues: TokenValue list) =
    tokenValues |> List.mapi (fun i x ->
        let pos = { index = i; line = 0; column = 0 }
        { value = x; start = pos; finish = pos })
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()


toTree (setTokenPos [ 
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
        Token (tok 0 (Text "Text1"))
        Token (tok 1 (PExp (Hole ("hello", []))))
        Scope
            { root = tok 2 (PExp (If ("cond1", [])))
              children = [
                Token (tok 3 (Text "cond1_Text1"))
                Token (tok 4 (Text "cond1_Text2"))
                Scope
                    { root = tok 5 (PExp (For ("x", ("y", []))))
                      children = [
                        Token (tok 6 (Text "cond1_For1_Text1"))
                        Token (tok 7 (Text "cond1_For1_Text2"))
                        ]
                    }
                Token (tok 9 (Text "cond1_Text3"))
                ]
            }
        Token (tok 11 (Text "Text2"))
    ]


// Fails because of unclosed scope
toTree (setTokenPos [
    PExp(If ("cond1", []))
    Text "Text1"
    ])

// Fails because of unopened scope
toTree (setTokenPos [
    PExp(If ("cond1", []))
    Text "Text1"
    PExp End
    PExp End
    ])
