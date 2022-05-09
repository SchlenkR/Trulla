
#r "nuget: FParsec"
open FParsec

#load "parsing.fs"
open Trulla.Parsing

let test p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg
let shouldEqual expected str =
    let actual = test template str
    if expected <> actual then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
let shouldFail str =
    match run template str with
    | Success (result, _, _) ->
        failwith $"Expected failure. Was: {result}"
    | Failure (_, _, _) -> ()




"""abc {{ hello }} def {{xyz}}"""
|> shouldEqual [ Text "abc "; PExp(Hole ("hello", [])); Text " def "; PExp(Hole ("xyz", [])) ]


"""abc"""
|> shouldEqual [ Text "abc" ]


"""abc {{ """
|> shouldFail



"""abc {{ }}"""
|> shouldFail


"""abc {{ x {{"""
|> shouldFail



""" {{ x}}"""
|> shouldEqual [Text " "; PExp(Hole ("x", [])) ]


"""{{x}}"""
|> shouldEqual [ PExp(Hole ("x", [])) ]



"""abc {{ if x }}"""
|> shouldEqual [ Text "abc "; PExp(If ("x", [])) ]



"""abc {{ for x in y }}"""
|> shouldEqual [ Text "abc "; PExp(For ("x", ("y",[]))) ]


// TODO: Test {{{ (triple)


