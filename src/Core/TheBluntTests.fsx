
#load "TheBlunt.fs"
open TheBlunt



blank |> run "   " |> Expect.ok " "
blank |> run " "   |> Expect.ok " "
blank |> run "x"   |> Expect.error
blank |> run ""    |> Expect.error

blanks 1 |> run "     xxx" |> Expect.ok "     "
blanks 1 |> run "  xxx"    |> Expect.ok "  "
blanks 1 |> run " xxx"     |> Expect.ok " "
blanks 1 |> run "xxx"      |> Expect.error
blanks 0 |> run "xxx"      |> Expect.ok ""

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

manyStr (%"ab")
|> run "abababX"
|> Expect.ok "ababab"

manyStr (%"ab")
|> run ""
|> Expect.ok ""

many1Str (%"ab")
|> run ""
|> Expect.error

many1Str (%"ab")
|> run "abababX"
|> Expect.ok "ababab"

let sepBy psep (pelem: Parser<_,_>) =
    parseItems {
        for x in pelem do
            yield x
            let! sepOk = pisOk psep
            if not sepOk then
                yield! Break
    }

%"ab" |> sepBy %";"
|> run "ab;ab;ab"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> sepBy %";"
|> run "ab;ab;abX"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> sepBy %";"
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> sepBy %";"
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

let sepBy1 psep (pelem: Parser<_,_>) =
    parseItems {
        yield! pelem
        for x in psep >>. pelem do
            yield x
    }

%"ab" |> sepBy1 %";"
|> run "ab;ab;ab"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> sepBy1 %";"
|> run "ab;ab;abX"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> sepBy1 %";"
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

%"ab" |> sepBy1 %";"
|> run "ab;ab;ab;"
|> Expect.ok ["ab"; "ab"; "ab" ]

parse {
    for x in anyChar do
        if x = "a" || x = "b" || x = "c" then
            yield x
        elif x = "X" then
            yield! Break
}
|> run "bacdeaXabb"
|> Expect.ok "baca"

// while Tests
parse {
    let mutable i = 0
    while true do
        yield $"{i}"
        i <- i + 1
        if i = 10 then
            yield! Break
}
|> run ""
|> Expect.ok "0123456789"

// while Tests
parse {
    let mutable i = 0
    while i < 10 do
        yield $"{i}"
        i <- i + 1
}
|> run ""
|> Expect.ok "0123456789"
