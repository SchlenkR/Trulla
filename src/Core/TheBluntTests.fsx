
#load "TheBlunt.fs"
open TheBlunt

module Expect =
    let ok expected res =
        match res with
        | Ok res ->
            printfn "Result: %A" res
            if res <> expected then
                failwithf "Expected: %A, but got: %A" expected res
        | Error err -> failwithf "Expected: %A, but got error: %A" expected err
    let error res =
        match res with
        | Ok res -> failwithf "Expected to fail, but got: %A" res
        | Error _ -> ()


blank |> run "   " |> Expect.ok " "
blank |> run " "   |> Expect.ok " "
blank |> run "x"   |> Expect.error
blank |> run ""    |> Expect.error

blanks1 |> run "     xxx" |> Expect.ok "     "
blanks1 |> run "  xxx"    |> Expect.ok "  "
blanks1 |> run " xxx"     |> Expect.ok " "
blanks1 |> run "xxx"      |> Expect.error
blanks  |> run "xxx"      |> Expect.ok ""

// TODO: Test

letter
|> run "abc"
|> Expect.ok "a"

letter
|> run "1abc"
|> Expect.error

digit
|> run "1abc"
|> Expect.ok "1"

digit
|> run "abc"
|> Expect.error

pstrNotFollowedBy "ab" "c"
|> run "abc"
|> Expect.error

pstrNotFollowedBy "ab" "d"
|> run "abc"
|> Expect.ok "ab"

many (%"ab") |> pconcat |> noRange
|> run "abababX"
|> Expect.ok "ababab"

many (%"ab") |> pconcat |> noRange
|> run ""
|> Expect.ok ""

many1Str (%"ab")
|> run ""
|> Expect.error

many1Str (%"ab")
|> run "abababX"
|> Expect.ok "ababab"

%"ab" |> psepBy %";" |> noRanges
|> run "ab;ab;ab"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> psepBy %";" |> noRanges
|> run "ab;ab;abX"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> psepBy %";" |> noRanges
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> psepBy %";" |> noRanges
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> psepBy1 %";" |> noRanges
|> run "ab;ab;ab"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> psepBy1 %";" |> noRanges
|> run "ab;ab;abX"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> psepBy1 %";" |> noRanges
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> psepBy1 %";" |> noRanges
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

// should compile: Transition between ForStates
let forStateTransitionTests () =
    let inline ident () = many1Str2 letter (letter <|> digit)
    let propAccess = parse {
        let! segments = ident () |> psepBy1 %"."
        return segments
    }
    parse {
        let! identExp = pstr "for" >>. blanks1 >>. ident () .>> blanks1
        return identExp
    }

pchoice [
    pstr "a"
    pstr "b"
    pstr "c"
]
|> run "cab"
|> Expect.ok "c"

pchoice [
    pstr "a"
    pstr "b"
    pstr "c"
]
|> run "xyz"
|> Expect.error

