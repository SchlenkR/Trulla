module Trulla.Internal.Parsing

open FParsec

type Position = { index: int64; line: int64; column: int64 } with
    override this.ToString() = $"I{this.index}"
type Range = { start: Position; finish: Position } with
    override this.ToString() = $"{this.start}-{this.finish}"
type PVal<'a> = { range: Range; value: 'a } with
    override this.ToString() = $"({this.range}){this.value}"
type TrullaError = { ranges: Range list; message: string }
exception TrullaException of TrullaError

type ParserToken =
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

module PVal =
    let create range value = { PVal.value = value; range = range }

module Position =
    let none = { index = -1L; line = -1L; column = -1L }
    let zero = { index = 0L; line = 0L; column = 0L }
    let ofFParsec offset (p: FParsec.Position) =
        { index = p.Index - offset; line = p.Line; column = p.Column - offset }
    let toRange (pos: Position) = { start = pos; finish = pos }

module Exp =
    let createFromSegments (segments: PVal<string> list) =
        match segments with
        | x::xs ->
            ({ range = x.range; value = IdentExp x }, xs)
            ||> List.fold (fun state x ->
                let accExp = AccessExp {| instanceExp = state; memberName = x.value |}
                { range = x.range; value = accExp }
            )
        | [] -> failwith "Should never happen: Information loss in sepBy1 parser."

[<AutoOpen>]
module ParserHelper =
    /// Wrap a token parser to include the position.
    let withPos parser =
        let leftOf (p: FParsec.Position) =
            p |> Position.ofFParsec (if p.Column > 1L then 1L else 0L)
        pipe3 getPosition parser getPosition <| fun start value finish ->
            { value = value
              range = {
                  start = Position.ofFParsec 0L start
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
            withPos (
                pstring Keywords.for' >>. blanks1 >>. withPos ident .>> blanks1 .>> 
                pstring Keywords.in' .>> blanks1 .>>. propAccess
            )
            |>> fun x -> PVal.create x.range (For x.value)
        let ifExp = 
            withPos (pstring Keywords.if' >>. blanks1 >>. propAccess)
            |>> fun x -> PVal.create x.range (If x.value)
        //let elseIfExp = pstring Keywords.elseIf' >>. blanks1 >>. propAccess |>> ElseIf
        //let elseExp = pstring Keywords.else' |>> fun _ -> Else
        let endExp = 
            withPos (pstring Keywords.end')
            |>> fun x -> PVal.create x.range (End)
        let holeExp = 
            withPos propAccess
            |>> fun x -> PVal.create x.range (Hole x.value)
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
        withPos (chars1Until beginExp) |>> fun x -> PVal.create x.range (Text x.value)
        withPos (many1Chars anyChar) |>> fun x -> PVal.create x.range (Text x.value)
        ]
let ptemplate = many expOrText .>> eof

let parseTemplate templateString =
    match run ptemplate templateString with
    | Success (tokenList,_,_) -> Result.Ok tokenList
    | Failure (msg,error,_) ->
        { ranges = [Position.ofFParsec 0L error.Position |> Position.toRange]
          message = msg }
        |> List.singleton
        |> Result.Error
    
