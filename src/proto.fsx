
#r "nuget: FParsec"

open System
open FParsec

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

// this does NOT consume endp, but only tests for it.
let chars1Until endp = many1Chars (notFollowedBy endp >>. anyChar)

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
let beginExpr = pstring Consts.beginExpr .>> notFollowedBy (pstring "{")
let tmplExpr =
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

    beginExpr .>> blanks >>. body .>> blanks .>> endExpr |>> Expr
let exprOrText = 
    choice [
        tmplExpr                                                   // <!> "tmpl expression"
        chars1Until beginExpr |>> Text                             // <!> "chars til beginExp"
        many1Chars anyChar |>> Text                                // <!> "text only"
    ]
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




"""abc {{ hello }} def {{xyz}}"""
|> shouldEqual [ Text "abc "; Expr(Fill ["hello"]); Text " def "; Expr(Fill ["xyz"]) ]


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


// TODO: Test {{{ (triple)

