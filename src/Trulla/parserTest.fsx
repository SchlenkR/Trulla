
#r "nuget: FParsec"
open FParsec

#load "parsing.fs"
open Trulla.Parsing

let test p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg
let clearTokenPos (tokens: TmplToken list) =
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
        Text "abc "
        Hole ("hello", [])
        Text " def "
        Hole ("xyz", [])
    ]


"""abc"""
|> shouldEqual [ Text "abc" ]


"""abc {{ """
|> shouldFail



"""abc {{ }}"""
|> shouldFail


"""abc {{ x {{"""
|> shouldFail



""" {{ x}}"""
|> shouldEqual [Text " "; Hole ("x", []) ]


"""{{x}}"""
|> shouldEqual [ Hole ("x", []) ]



"""abc {{ if x }}"""
|> shouldEqual [ Text "abc "; If ("x", []) ]



"""abc {{ for x in y }}"""
|> shouldEqual [ Text "abc "; For ("x", ("y",[])) ]


// TODO: Test {{{ (triple)


