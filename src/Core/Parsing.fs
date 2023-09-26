namespace Trulla.Core

type PVal<'a> = { range: Range; value: 'a }

[<RequireQualifiedAccess>]
type Token =
    | Text of string
    | Hole of PVal<MemberToken>
    | For of ident: PVal<string> * exp: PVal<MemberToken> * sep: PVal<string option>
    | If of PVal<MemberToken>
    //| ElseIf of Access
    | Else
    | End

and MemberToken =
    | AccessToken of {| instanceExp: PVal<MemberToken>; memberName: string |}
    | IdentToken of string

type ParseResult = Result<PVal<Token> list, TrullaError list>

[<RequireQualifiedAccess>]
module Parsing =
    open TheBlunt

    module internal Consts =
        let beginExp = "{{"
        let endExp = "}}"

    module internal Keywords =
        let for' = "for"
        let in' = "in"
        let sep = "|"
        let if' = "if"
        let elseIf' = "else if"
        let else' = "else"
        let end' = "end"

    module internal Position =
        let toRange (pos: Trulla.Core.Position) = { start = pos; finish = pos }
    
    module internal Range =
        let merge ranges =
            let pick mapping weighter =
                ranges
                |> List.map mapping
                |> weighter (fun (x: Position) -> x.index)
                |> List.head
            {
                start = pick (fun x -> x.start) List.sortBy
                finish = pick (fun x -> x.finish) List.sortByDescending
            }

    module internal PVal =
        let create ranges value =
            { PVal.value = value; range = Range.merge ranges }

    module internal MemberToken =
        let createFromSegments (segments: PVal<string> list) =
            match segments with
            | x::xs ->
                ({ range = x.range; value = IdentToken x.value }, xs)
                ||> List.fold (fun state x ->
                    let accExp = AccessToken {| instanceExp = state; memberName = x.value |}
                    { range = x.range; value = accExp }
                )
            | [] -> failwith "Should never happen: Information loss in sepBy1 parser."

    [<AutoOpen>]
    module internal Internal =
        // let withPos parser =
        //     pipe3 getPosition parser getPosition <| fun start value finish ->
        //         { value = value
        //           range = {
        //               start = Position.ofFParsec 0L start
        //               finish = leftOf finish 
        //           }
        //         }

        let begin' = pstr Consts.beginExp .>> pnot (pstr "{")
        let tmplExp =
            let endExp = pstr Consts.endExp
            let ident = many1Chars2 letter (letter <|> digit)
            let propAccess =
                sepBy1 (withPos  ident) (pchar '.')
                |>> fun segments -> MemberToken.createFromSegments segments
            let body =
                let for' = parse {
                    let! identExp = pstr Keywords.for' >>. blanks 1 >>. withPos ident .>> blanks 1
                    let! memberExp = pstr Keywords.in' >>. blanks 1 >>. propAccess
                    let! sepExp = 
                        withPos(
                            (blanks >>. pstr Keywords.sep >>. manyCharsUntil endExp |>> Some)
                            <|>
                            (blanks |>> fun _ -> None)
                        )
                    return
                        Token.For (identExp, memberExp, sepExp)
                        |> PVal.create [ identExp.range; memberExp.range; sepExp.range ]
                }
                let if' = 
                    withPos (pstr Keywords.if' >>. blanks1 >>. propAccess)
                    |>> fun x -> PVal.create [x.range] (Token.If x.value)
                //let elseIfExp = pstr Keywords.elseIf' >>. blanks1 >>. propAccess |>> ElseIf
                let elseExp = 
                    withPos (pstr Keywords.else')
                    |>> fun x -> PVal.create [x.range] (Token.Else)
                let end' = 
                    withPos (pstr Keywords.end')
                    |>> fun x -> PVal.create [x.range] (Token.End)
                let hole = 
                    withPos propAccess
                    |>> fun x -> PVal.create [x.range] (Token.Hole x.value)
                choice [
                    for'
                    if'
                    ////elseIfExp
                    elseExp
                    end'
                    hole
                    ]
            begin' .>> blanks >>. body .>> blanks .>> endExp
        let expOrText =
            choice [
                tmplExp
                withPos (manyChars1Until begin') |>> fun x -> PVal.create [x.range] (Token.Text x.value)
                withPos (many1Chars anyChar) |>> fun x -> PVal.create [x.range] (Token.Text x.value)
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
