module Trulla.Parsing

open FParsec

type Position = { index: int64; line: int64; column: int64 }
type PositionalValue<'a> = { value: 'a; start: Position; finish: Position }

type ParseResult = PositionalValue<ParserToken> list
and ParserToken =
    | Text of string
    | Hole of AccessExp
    | For of ident: string * exp: AccessExp
    | If of AccessExp
    //| ElseIf of Access
    //| Else
    | End
and AccessExp = { ident: string; propPath: string list }

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

module Position =
    let none = { index = -1L; line = -1L; column = -1L }

[<AutoOpen>]
module ParserHelper =
    /// Wrap a token parser to include the position.
    let withPos (p: Parser<_,_>) : Parser<_,_> =
        let posFromFParsec offset (p: FParsec.Position) =
            { index = p.Index - offset; line = p.Line; column = p.Column - offset }
        let leftOf (p: FParsec.Position) =
            let offset = if p.Column > 1L then 1L else 0L
            p |> posFromFParsec offset
        pipe3 getPosition p getPosition (fun start value finish ->
            value, posFromFParsec 0L start, leftOf finish)
    let (|..>) parser f = parser |> withPos |>> fun (value, start, finish) ->
        { value = f value; start = start; finish = finish }
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
    let propAccess =
        sepBy1 ident (pchar '.')
        |>> function
            | (root :: rest) -> { ident = root; propPath = rest }
            | _ -> failwith "Should never happen: information loss in sepBy1 parser"
    let body =
        let forExp =
            pstring Keywords.for' >>. blanks1 >>. ident .>> blanks1 .>> pstring Keywords.in' .>> blanks1 .>>. propAccess
            |..> For
        let ifExp = 
            pstring Keywords.if' >>. blanks1 >>. propAccess
            |..> If
        //let elseIfExp = pstring Keywords.elseIf' >>. blanks1 >>. propAccess |>> ElseIf
        //let elseExp = pstring Keywords.else' |>> fun _ -> Else
        let endExp = 
            pstring Keywords.end' 
            |..> (fun _ -> End)
        let fillExp = 
            propAccess
            |..> Hole
        choice [ 
            forExp
            ifExp
            ////elseIfExp
            ////elseExp
            endExp
            fillExp
            ]
    beginExp .>> blanks >>. body .>> blanks .>> endExp
let expOrText = 
    choice [
        tmplExp
        chars1Until beginExp |..> Text
        many1Chars anyChar |..> Text
        ]
let template = many expOrText .>> eof
