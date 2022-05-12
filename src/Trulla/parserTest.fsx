
#r "nuget: FParsec"
open FParsec

#load "parsing.fs"
open Trulla.Parsing

let test p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg
let tok t = 
    { value = t; start = Position.none; finish = Position.none }
let accessExp ident propPath = 
    tok { ident = ident; propPath = propPath }
let shouldEqual (expected: ParserToken list) str =
    let clearPos p = { p with start = Position.none; finish = Position.none }
    let actual = 
        [ for token in test template str do
            match token with
            | Text _ -> token
            | Hole x -> Hole (clearPos x)
            | For (i,s) -> For (clearPos i, clearPos s)
            | If x -> If (clearPos x)
            | End -> token
        ]
    if expected <> actual then
        failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
let shouldFail str =
    match run template str with
    | Success (result, _, _) -> failwith $"Expected failure. Was: {result}"
    | Failure (_, _, _) -> ()



"""abc {{ hello }} def {{xyz}}"""
|> shouldEqual
    [
        Text "abc "
        Hole (accessExp "hello" [])
        Text " def "
        Hole (accessExp "xyz" [])
    ]


"""abc"""
|> shouldEqual
    [
        Text "abc"
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
        Text " "
        Hole (accessExp "x" [])
    ]


"""{{x}}"""
|> shouldEqual
    [ 
        Hole (accessExp "x" [])
    ]



"""abc {{ if x }}"""
|> shouldEqual 
    [ 
        Text "abc "
        If (accessExp "x" [])
    ]



"""abc {{ for x in y }}"""
|> shouldEqual 
    [
        Text "abc "
        For (tok "x", accessExp "y" [])
    ]


// TODO: Test {{{ (triple)
