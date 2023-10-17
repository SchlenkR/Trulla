namespace Trulla.Core

open TheBlunt

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
        let templateExp =
            let endExp = pstr Consts.endExp
            let inline ident () = many1Str2 letter (letter <|> digit)
            let propAccess = parse {
                let! segments = ident () |> psepBy1 %"."
                return MemberToken.createFromSegments segments.result
            }
            let body =
                let for' = parse {
                    let! identExp = pstr Keywords.for' >>. blanks1 >>. ident () .>> blanks1
                    let! memberExp = pstr Keywords.in' >>. blanks1 >>. propAccess
                    let! sepExp = 
                        (blanks >>. pstr Keywords.sep >>. pstringUntil endExp |> map Some)
                        <|>
                        (blanks |> map (fun _ -> None))
                    return
                        {
                            range = Range.merge [ identExp.range; memberExp.range; sepExp.range ]
                            result = Token.For (identExp, memberExp, sepExp)
                        }
                }
                let if' = 
                    (pstr Keywords.if' >>. blanks1 >>. propAccess)
                    |> mapPVal Token.If
                //let elseIfExp = pstr Keywords.elseIf' >>. blanks1 >>. propAccess |>> ElseIf
                let elseExp = 
                    pstr Keywords.else'
                    |> map (fun _ -> Token.Else)
                let end' = 
                    pstr Keywords.end'
                    |> map (fun _ -> Token.End)
                let hole = 
                    propAccess
                    |> mapPVal Token.Hole
                pchoice
                    [
                        for'
                        if'
                        ////elseIfExp
                        elseExp
                        end'
                        hole
                    ]
            begin' .>> blanks >>. body .>> blanks .>> endExp
        let expOrText =
            pchoice 
                [
                    templateExp
                    pstringUntil begin' |> map Token.Text
                    anyChar |> map Token.Text
                ]
        let ptemplate = many expOrText .>> eoi

    let parseTemplate templateString = run templateString ptemplate
