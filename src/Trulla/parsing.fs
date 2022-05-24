module Trulla.Parsing

open FParsec

type Position = { index: int64; line: int64; column: int64 }
type Range = { start: Position; finish: Position }
type PVal<'a> = { value: 'a; range: Range }

type ParseResult = ParserToken list
and ParserToken =
    | Text of string
    | Hole of PVal<Exp>
    | For of ident: PVal<string> * exp: PVal<Exp>
    | If of PVal<Exp>
    //| ElseIf of Access
    //| Else
    | End
and Exp =
    | AccessExp of {| instanceExp: PVal<Exp>; memberName: string |}
    | IdentExp of PVal<string>

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
    let right offset pos = { pos with index = pos.index + offset; column = pos.column + offset }

module Exp =
    let createFromSegments =
        function
        | x::xs ->
            ({ range = x.range; value = IdentExp x }, xs)
            ||> List.fold (fun state x ->
                let accExp = AccessExp {| instanceExp = state; memberName = x.value |}
                { range = x.range; value = accExp }
            )
        | [] -> failwith "Should never happen: information loss in sepBy1 parser"

[<AutoOpen>]
module ParserHelper =
    /// Wrap a token parser to include the position.
    let withPos parser =
        let posFromFParsec offset (p: FParsec.Position) =
            { index = p.Index - offset; line = p.Line; column = p.Column - offset }
        let leftOf (p: FParsec.Position) =
            let offset = if p.Column > 1L then 1L else 0L
            p |> posFromFParsec offset
        pipe3 getPosition parser getPosition <| fun start value finish ->
            { value = value
              range = {
                  start = posFromFParsec 0L start
                  finish = leftOf finish 
              }
            }
    ////let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    ////    fun stream ->
    ////        printfn "%A: Entering %s" stream.Position label
    ////        let reply = p stream
    ////        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
    ////        reply

    /// This does NOT consume endp, but only tests for it.
    let chars1Until endp = many1Chars (notFollowedBy endp >>. anyChar)

    let blanks : Parser<_, unit> = skipMany (skipChar ' ')
    let blanks1 : Parser<_, unit> = skipMany1 (skipChar ' ')

let beginExp = pstring Consts.beginExp .>> notFollowedBy (pstring "{")
let tmplExp =
    let endExp = pstring Consts.endExp
    let ident = many1Chars2 letter (letter <|> digit)
    let propAccess =
        sepBy1 (withPos  ident) (pchar '.')
        |>> fun segments -> Exp.createFromSegments segments

    let body =
        let forExp =
            pstring Keywords.for' >>. blanks1 >>. withPos ident .>> blanks1 .>> pstring Keywords.in' .>> blanks1 .>>. propAccess
            |>> For
        let ifExp = 
            pstring Keywords.if' >>. blanks1 >>. propAccess
            |>> If
        //let elseIfExp = pstring Keywords.elseIf' >>. blanks1 >>. propAccess |>> ElseIf
        //let elseExp = pstring Keywords.else' |>> fun _ -> Else
        let endExp = 
            pstring Keywords.end' 
            |>> (fun _ -> End)
        let holeExp = propAccess |>> Hole
        choice [
            forExp
            ifExp
            ////elseIfExp
            ////elseExp
            endExp
            holeExp
            ]
    beginExp .>> blanks >>. body .>> blanks .>> endExp
let expOrText = 
    choice [
        tmplExp
        chars1Until beginExp |>> Text
        many1Chars anyChar |>> Text
        ]
let template = many expOrText .>> eof
