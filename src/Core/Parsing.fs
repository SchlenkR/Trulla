namespace Trulla.Core

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

    open TheBlunt

    module PVal =
        let map proj (p: PVal<'a>) = { range = p.range; result = proj p.result }

    module internal MemberToken =
        let createFromSegments (segments: PVal<string> list) =
            match segments with
            | seg1 :: segs ->
                let identTok = seg1 |> PVal.map IdentToken
                let rec mkAccToks curr segs =
                    match segs with
                    | seg :: segs ->
                        let accTok = AccessToken {| instanceExp = curr; memberName = seg.result |}
                        let pvalAccTok = { range = seg.range; result = accTok }
                        mkAccToks pvalAccTok segs
                    | [] -> curr
                mkAccToks identTok segs
            | [] -> failwith "Should never happen: Information loss in sepBy1 parser."

    [<AutoOpen>]
    module internal Internal =

        let begin' = pstr Consts.beginExp .>> pnot (pstr "{")
        let tmplExp =
            let endExp = pstr Consts.endExp
            let inline ident () = many1Str2 letter (letter <|> digit)
            let propAccess = parse {
                let! segments = ident () |> psepBy1 %"."
                return MemberToken.createFromSegments segments
            }
            let body =
                let for' = parse {
                    let! identExp = pstr Keywords.for' >>. blanks 1 >>. withRange (ident ()) .>> blanks 1
                    let! memberExp = pstr Keywords.in' >>. blanks 1 >>. propAccess
                    let! sepExp = 
                        withRange(
                            (blanks 0 >>. pstr Keywords.sep >>. pstringUntil endExp |> map Some)
                            <|>
                            (blanks 0 |> map (fun _ -> None))
                        )
                    return
                        Token.For (identExp, memberExp, sepExp)
                        |> PVal.create [ identExp.range; memberExp.range; sepExp.range ]
                }
                let if' = 
                    withRange (pstr Keywords.if' >>. blanks 1 >>. propAccess)
                    |>map (fun x -> PVal.create [x.range] (Token.If x.value))
                //let elseIfExp = pstr Keywords.elseIf' >>. blanks1 >>. propAccess |>> ElseIf
                let elseExp = 
                    withRange (pstr Keywords.else')
                    |> map (fun x -> PVal.create [x.range] (Token.Else))
                let end' = 
                    withRange (pstr Keywords.end')
                    |> map (fun x -> PVal.create [x.range] (Token.End))
                let hole = 
                    withRange propAccess
                    |> map (fun x -> PVal.create [x.range] (Token.Hole x.value))
                pchoice [
                    for'
                    if'
                    ////elseIfExp
                    elseExp
                    end'
                    hole
                    ]
            begin' .>> blanks 0 >>. body .>> blanks 0 .>> endExp
        let expOrText =
            pchoice [
                tmplExp
                withRange (manyChars1Until begin') |> map (fun x -> PVal.create [x.range] (Token.Text x.value))
                withRange (many1Chars anyChar) |> map (fun x -> PVal.create [x.range] (Token.Text x.value))
                ]
        let ptemplate = many expOrText .>> eoi

    let parseTemplate templateString =
        match run ptemplate templateString with
        | Success (tokenList,_,_) -> Result.Ok tokenList
        | Failure (msg,error,_) ->
            { ranges = [Position.ofFParsec 0L error.Position |> Position.toRange]
              message = msg }
            |> List.singleton
            |> Result.Error
