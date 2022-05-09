
#r "nuget: FParsec"
open FParsec

#load "parser.fs"
open Trulla

let template = Parser.template

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
|> shouldEqual [ Text "abc "; Expr(Hole ["hello"]); Text " def "; Expr(Hole ["xyz"]) ]


"""abc"""
|> shouldEqual [ Text "abc" ]


"""abc {{ """
|> shouldFail



"""abc {{ }}"""
|> shouldFail


"""abc {{ x {{"""
|> shouldFail



""" {{ x}}"""
|> shouldEqual [Text " "; Expr(Hole ["x"]) ]


"""{{x}}"""
|> shouldEqual [ Expr(Hole ["x"]) ]



"""abc {{ if x }}"""
|> shouldEqual [ Text "abc "; Expr(If ["x"]) ]



"""abc {{ for x in y }}"""
|> shouldEqual [ Text "abc "; Expr(For {| ident = "x"; source = ["y"]; |}) ]


// TODO: Test {{{ (triple)


