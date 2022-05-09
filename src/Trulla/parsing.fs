module Trulla.Parsing

open FParsec

// TODO: IfNot
// TODO: Col+Row -> meaningful error messages in Typing
type Token =
    | Text of string
    | PExp of PExp
and PExp =
    | Hole of Access
    | For of ident: string * source: Access
    | If of Access
    | ElseIf of Access
    | Else
    | End
and Access = string * string list

module Consts =
    let beginExp = "{{"
    let endExp = "}}"

module Keywords =
    let for' = "for"
    let in' = "in"
    let if' = "if"
    let elseIf' = "else if"
    let else' = "else"
    let end' = "end"

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply
    
// this does NOT consume endp, but only tests for it.
let chars1Until endp = many1Chars (notFollowedBy endp >>. anyChar)
let blanks : Parser<_, unit> = skipMany (skipChar ' ')
let blanks1 : Parser<_, unit> = skipMany1 (skipChar ' ')

let beginExp = pstring Consts.beginExp .>> notFollowedBy (pstring "{")
let tmplExp =
    let endExp = pstring Consts.endExp
    let ident = many1Chars2 letter (letter <|> digit)
    let propAccess = sepBy1 ident (pchar '.') |>> fun (root :: rest) -> root,rest
    let body =
        let forExp =
            pstring Keywords.for' >>. blanks1 >>. ident .>> blanks1 .>> pstring Keywords.in' .>> blanks1 .>>. propAccess
            |>> fun (ident,source) -> For (ident, source)
        let ifExp = pstring Keywords.if' >>. blanks1 >>. propAccess |>> If
        let elseIfExp = pstring Keywords.elseIf' >>. blanks1 >>. propAccess |>> ElseIf
        let elseExp = pstring Keywords.else' |>> fun _ -> Else
        let endExp = pstring Keywords.end' >>. preturn End
        let fillExp = propAccess |>> Hole
        choice [ forExp; ifExp; elseIfExp; elseExp; endExp; fillExp ]
    beginExp .>> blanks >>. body .>> blanks .>> endExp |>> PExp
let expOrText = 
    choice [
        tmplExp                                                   // <!> "tmpl expression"
        chars1Until beginExp |>> Text                             // <!> "chars til beginExp"
        many1Chars anyChar |>> Text                               // <!> "text only"
    ]
let template = many expOrText .>> eof
