
#r "nuget: FParsec"
open FParsec

#load "parsing.fs"
open Trulla.Parsing

let test p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg
let clearTokenPos tokens =
    [ for t in tokens do { t with start = Position.none; finish = Position.none } ]
let tok t = { value = t; start = Position.none; finish = Position.none }
let shouldEqual expected str =
    let actual = test template str |> clearTokenPos
    let expected = expected |> List.map tok
    if expected <> actual then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
let shouldFail str =
    match run template str with
    | Success (result, _, _) -> failwith $"Expected failure. Was: {result}"
    | Failure (_, _, _) -> ()



"""abc {{ hello }} def {{xyz}}"""
|> shouldEqual
    [
        LeafToken (Text "abc ")
        LeafToken (Hole ("hello", []))
        LeafToken (Text " def ")
        LeafToken (Hole ("xyz", []))
    ]


"""abc"""
|> shouldEqual
    [
        LeafToken (Text "abc")
    ]


"""abc {{ """
|> shouldFail



"""abc {{ }}"""
|> shouldFail


"""abc {{ x {{"""
|> shouldFail



""" {{ x}}"""
|> shouldEqual
    [
        LeafToken (Text " ")
        LeafToken (Hole ("x", []))
    ]


"""{{x}}"""
|> shouldEqual
    [ 
        LeafToken (Hole ("x", []))
    ]



"""abc {{ if x }}"""
|> shouldEqual 
    [ 
        LeafToken (Text "abc ")
        ScopeToken (If ("x", []))
    ]



"""abc {{ for x in y }}"""
|> shouldEqual 
    [
        LeafToken (Text "abc ")
        ScopeToken (For ("x", ("y",[])))
    ]


// TODO: Test {{{ (triple)


