
#r "nuget: FParsec"

open System
open FParsec

type Token =
    | Text of string
    | Expr of Expr
and Expr =
    | Fill of PropAccess
    | For of {| ident: string; source: PropAccess |}
    | If of PropAccess
    | End
and PropAccess = string list

module Consts =
    let beginExpr = "{{"
    let endExpr = "}}"

type PUnit<'a> = Parser<'a, unit>

let blanks : PUnit<_> = skipMany (skipChar ' ')
let blanks1 : PUnit<_> = skipMany1 (skipChar ' ')
let tmplExpr =
    let beginExpr = pstring Consts.beginExpr
    let endExpr = pstring Consts.endExpr
    
    let ident = many1Chars2 letter (letter <|> digit)
    let propAccess = sepBy1 ident (pchar '.')

    let body =
        let forExpr =
            pstring "for" >>. blanks1 >>. ident .>> blanks1 .>> pstring "in" .>> blanks1 .>>. propAccess
            |>> fun (x,y) -> For {| ident = x; source = y |}
        let ifExpr = pstring "if" >>. blanks1 >>. propAccess |>> If
        let endExpr = pstring "/" >>. preturn End
        let fillExpr = propAccess |>> Fill

        choice [ forExpr; ifExpr; endExpr; fillExpr ]

    beginExpr >>. blanks >>. body .>> blanks .>> endExpr |>> Expr
let textOnly = many1Chars anyChar |>> Text
let textBeforeExpr = charsTillString "{{" false Int32.MaxValue |>> Text
let exprOrText = choice [ tmplExpr; attempt textBeforeExpr; textOnly ]
let template = many exprOrText .>> eof



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




"""abc {{ hello }} def {{xx}}"""
|> shouldEqual [ Text "abc "; Expr(Fill ["hello"]); Text " def "; Expr(Fill ["xx"]) ]


"""abc"""
|> shouldEqual [ Text "abc" ]


"""abc {{ """
|> shouldFail



"""abc {{ }}"""
|> shouldFail


"""abc {{ x {{"""
|> shouldFail



""" {{ x}}"""
|> shouldEqual [Text " "; Expr(Fill ["x"]) ]


"""{{x}}"""
|> shouldEqual [ Expr(Fill ["x"]) ]



"""abc {{ if x }}"""
|> shouldEqual [ Text "abc "; Expr(If ["x"]) ]



"""abc {{ for x in y }}"""
|> shouldEqual [ Text "abc "; Expr(For {| ident = "x"; source = ["y"]; |}) ]

