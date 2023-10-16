module TheBlunt

open System
open System.Runtime.CompilerServices

type Str =
    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    System.ReadOnlySpan<char>
    #else
    System.String
    #endif

[<Extension>]
type StringExtensions =

    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    [<Extension>] 
    static member inline StringEquals(s: Str, compareWith: string) = 
        s.SequenceEqual(compareWith.AsSpan())
    [<Extension>] 
    static member inline StringEquals(s: Str, compareWith: Str) =
        s.SequenceEqual(compareWith)
    [<Extension>]
    static member inline StringEquals(s: string, compareWith: Str)  =
        s.AsSpan().SequenceEqual(compareWith)
    #endif
    [<Extension>]
    static member inline StringEquals(s: string, compareWith: string) = 
        String.Equals(s, compareWith)

    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    [<Extension>]
    static member StringStartsWithAt(this: Str, other: Str, idx: int) =
        idx + other.Length <= this.Length
        && this.Slice(idx, other.Length).StringEquals(other)
    [<Extension>] 
    static member StringStartsWithAt(this: Str, other: string, idx: int) =
        this.StringStartsWithAt(other.AsSpan(), idx)
    [<Extension>]
    static member StringStartsWithAt(this: string, other: string, idx: int) =
        this.AsSpan().StringStartsWithAt(other.AsSpan(), idx)
    #else
    [<Extension>]
    static member StringStartsWithAt(this: string, other: string, idx: int) =
        this.Substring(idx).StartsWith(other)
    #endif

    #if !FABLE_COMPILER && NETSTANDARD2_1_OR_GREATER
    [<Extension>]
    static member Slice(this: string, start: int) =
        this.AsSpan().Slice(start)
    #else
    [<Extension>]
    static member Slice(this: string, start: int) =
        this.Substring(start)
    #endif


// -----------------------------------------------------------------------------------------------
// BEGIN :)
// -----------------------------------------------------------------------------------------------


type ParserFunc<'value, 'state> = Cursor -> 'state -> ParserResult<'value>

and [<Struct>] Cursor =
    { original: string
      idx: int }

and [<Struct>] ParserResult<'out> =
    | POk of ok: PVal<'out>
    | PError of error: ParseError

and [<Struct>] PVal<'out> =
    { range: Range
      result: 'out }

and [<Struct>] ParseError =
    { idx: int
      message: string }

and [<Struct>] Range = 
    { startIdx: int
      endIdx: int }

type [<Struct>] DocPos =
    { idx: int
      ln: int
      col: int }

[<AutoOpen>]
module Inlining =
    #if INLINE_IF_LAMBDA

    type Parser<'value, 'state> = ParserFunction<'value, 'state>
    let inline mkParser (parser: Parser<_,_>) = parser
    let inline getParser (parser: Parser<_,_>) = parser
    module Inline = type IfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute

    #else

    type Parser<'value, 'state> = Parser of ParserFunc<'value, 'state>
    let inline mkParser parserFunction = Parser parserFunction
    let inline getParser (parser: Parser<_,_>) = let (Parser p) = parser in p
    module Inline = type IfLambdaAttribute() = inherit System.Attribute()

    #endif

type Cursor with
    member c.CanGoto(idx: int) =
        // TODO: Should be: Only forward
        idx >= c.idx && idx <= c.original.Length
    member c.CanWalkFwd(steps: int) = c.CanGoto(c.idx + steps)
    member c.IsAtEnd = c.idx = c.original.Length
    member c.HasRest = c.idx < c.original.Length
    member c.Rest : Str = c.original.Slice(c.idx)
    member c.StartsWith(s: string) = c.Rest.StringStartsWithAt(s, 0)
    member c.Goto(idx: int) =
        if not (c.CanGoto(idx)) then
            failwithf "Index %d is out of range of string of length %d." idx c.original.Length
        { idx = idx; original = c.original }
    member c.WalkFwd(steps: int) = c.Goto(c.idx + steps)
    member c.MoveNext() = c.WalkFwd(1)

module Range =
    let inline create startIdx endIdx = { startIdx = startIdx; endIdx = endIdx }
    let zero = { startIdx = 0; endIdx = 0 }
    let inline merge r1 r2 = { startIdx = r1.startIdx; endIdx = r2.endIdx }

module POk =
    let inline createFromRange range (result: 'a) : ParserResult<'a> =
        POk { range = range; result = result }
    let inline create startIdx endIdx (result: 'a) : ParserResult<'a> =
        createFromRange (Range.create startIdx endIdx) result

module PError =
    let inline create (idx: int) (message: string) : ParserResult<'a> =
        PError { idx = idx; message = message }

// TODO: Perf: The parser combinators could track that, instead of computing it from scratch.
module DocPos =
    let create (index: int) (input: string) =
        if index < 0 || index > input.Length then
            failwithf "Index %d is out of range of input string of length %d." index input.Length
        let lineStart = 1
        let columnStart = 1
        let mutable currIdx = 0
        let mutable line = lineStart
        let mutable column = columnStart
        while currIdx <> index do
            let isLineBreak = input.StringStartsWithAt("\n", currIdx)
            if isLineBreak then
                line <- line + 1
                column <- columnStart
            else
                column <- column + 1
            currIdx <- currIdx + 1
        { idx = index; ln = line; col = column }
    let ofInput (pi: Cursor) = create pi.idx pi.original

module Cursor =
    let hasRemainingChars<'s> n =
        fun (inp: Cursor) (state: 's) ->
            if not (inp.CanWalkFwd n)
            then PError.create inp.idx (sprintf "Expected %d more characters." n)
            else POk.create inp.idx inp.idx ()
    let notAtEnd parserFunc = 
        hasRemainingChars 1 parserFunc

let pwhen pred p =
    mkParser <| fun inp state ->
        match pred inp state with
        | PError err -> PError.create inp.idx err.message
        | POk _ -> getParser p inp state

let mkParserWhen pred pf = pwhen pred <| mkParser pf

let inline bind (f: PVal<'a> -> Parser<_,_>) (parser: Parser<_,_>) =
    mkParser <| fun inp state ->
        match getParser parser inp state with
        | PError error -> PError error
        | POk pRes ->
            let fParser = getParser (f pRes)
            fParser (inp.Goto(pRes.range.endIdx)) state

let preturn value =
    mkParser <| fun inp state -> POk.create inp.idx inp.idx value

let preturnError message =
    mkParser <| fun inp state -> PError.create inp.idx message

type ParserBuilder() =
    member inline _.Bind(p, f) = bind f p
    member _.Return(x) = preturn x
    member _.ReturnFrom(p: Parser<_,_>) = p

let parse = ParserBuilder()

let pseq (s: _ seq) =
    let enum = s.GetEnumerator()
    mkParser <| fun inp state ->
        if enum.MoveNext()
        then POk.create inp.idx inp.idx enum.Current
        else PError.create inp.idx "No more elements in sequence."

let inline run (text: string) (parser: Parser<_,_>) =
    match getParser parser { idx = 0; original = text} None with
    | POk res -> Ok res.result
    | PError error ->
        let docPos = DocPos.create error.idx text
        Error {| pos = docPos; message = error.message |}

// TODO: I guess that all the state stuff will turn out to be a quite unuseful,
// or compared to the 

let map proj p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | PError error -> PError error
        | POk pRes -> POk.createFromRange pRes.range (proj pRes.result)

let pignore p =
    p |> map (fun _ -> ())

let pattempt p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk.createFromRange res.range (Some res)
        | PError err -> PError err

let pisOk p = 
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk.createFromRange res.range true
        | PError err -> POk.create inp.idx inp.idx false

let pisErr p = pisOk p |> map not

// TODO: A strange thing is this
let pnot p =
    mkParser <| fun inp state ->
        match getParser (pattempt p) inp state with
        | POk _ -> PError.create inp.idx "Unexpected." // TODO
        | PError _ -> POk.create inp.idx inp.idx ()

let punit<'s> =
    mkParser <| fun inp (state: 's) ->
        POk.create inp.idx inp.idx ()

let pstr<'s> (s: string) =
    mkParser <| fun inp (state: 's) ->
        if inp.StartsWith(s)
        then POk.create inp.idx (inp.idx + s.Length) s
        else PError.create inp.idx (sprintf "Expected: '%s'" s)
let ( ~% ) = pstr

let pgoto (idx: int) =
    mkParser <| fun inp state ->
        if inp.CanGoto(idx) then 
            POk.create inp.idx idx ()
        else
            // TODO: this propably would be a fatal, most propably an unexpected error
            let msg = sprintf "Index %d is out of range of string of length %d." idx inp.original.Length
            PError.create idx msg

let orThen a b =
    mkParser <| fun inp state ->
        match getParser a inp state with
        | POk res -> POk res
        | PError _ -> getParser b inp state
let ( <|> ) a b = orThen a b

let inline andThen a b =
    mkParser <| fun inp state ->
        match getParser a inp state with
        | POk ares ->
            match getParser b (inp.Goto ares.range.endIdx) state with
            | POk bres -> POk.create inp.idx bres.range.endIdx (ares.result, bres.result)
            | PError error -> PError error
        | PError error -> PError error
// type AndThen = AndThen with
//     static member inline ($) (AndThen, x: (Parser<_,_> * Parser<_,_>)) =
//         let a,b = x
//         andThen a b
// let inline ( <&> ) a b = (($) AndThen) (a, b)
let ( .>. ) a b = andThen a b
let ( .>> ) a b = andThen a b |> map fst
let ( >>. ) a b = andThen a b |> map snd

let firstOf parsers = parsers |> List.reduce orThen

let rec many (p: Parser<_,_>) =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs.result
    }

let many1 (p: Parser<_,_>) =
    parse {
        let! res = many p
        match res.result with
        | [] -> return! preturnError "Expected at least one element."
        | _ -> return res.result
    }

// TODO: sepBy
// TODO: skipN

let anyChar<'s> =
    mkParserWhen Cursor.notAtEnd <| fun inp (state: 's) ->
        POk.create inp.idx (inp.idx + 1) (inp.Rest.[0].ToString())

// TODO: anyCharExcept(c,p)

let eoi<'s> : Parser<_,'s> =
    pwhen Cursor.notAtEnd <| preturn ()

let blank<'s> = pstr<'s> " "

/// Parse at least n or more blanks.
let blanks n =
    ()

let pstringUntil puntil =
    mkParser <| fun inp state ->
        let rec iter currIdx =
            match getParser (pattempt puntil) (inp.Goto currIdx) state with
            | POk _ -> POk.create inp.idx currIdx (inp.original.Substring(inp.idx, currIdx - inp.idx))
            | PError _ ->
                if not (inp.CanGoto(currIdx + 1))
                then PError.create currIdx "End of input."
                else iter (currIdx + 1)
        iter inp.idx

let concat (p: Parser<_,_>) =
    p |> map (fun x -> String.concat "" x)

// TODO: make clear: Parsers that 

let many1Str2 (p1: Parser<_,_>) (p2: Parser<_,_>) =
    parse {
        yield! p1
        for x in p2 do
            yield x
    }

let withErrorMessage msg p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk _ as res -> res
        | PError err -> PError { err with message = msg }

let many1Str (p: Parser<_,_>) = many1Str2 p p

let pchar<'s> predicate errMsg =
    mkParserWhen Cursor.notAtEnd <| fun inp (state: 's) ->
        let c = inp.Rest.[0]
        if predicate c
        then POk.create inp.idx (inp.idx + 1) (string c)
        else PError.create inp.idx (errMsg c)

let letter<'s> =
    pchar<'s> (Char.IsLetter) (sprintf "Expected letter, but got '%c'.")

let digit<'s> =
    pchar<'s> (Char.IsDigit) (sprintf "Expected letter, but got '%c'.")

let pstrNotFollowedBy s suffix =
    parse {
        let! x = pstr s
        let! _ = pnot (pstr suffix)
        return x.result
    }

let psepBy psep (pelem: Parser<_,_>) =
    parseItems {
        for x in pelem do
            yield x
            let! sepOk = pisOk psep
            if not sepOk.result then
                yield! Break
    }

let psepBy1 psep (pelem: Parser<_,_>) =
    parseItems {
        yield! pelem
        for x in psep >>. pelem do
            yield x
    }

let pchoice parsers =
    mkParser <| fun inp state ->
        let rec iter parsers =
            match parsers with
            | [] -> PError.create inp.idx "No more parsers to try." // TODO: Better: Expected ... or ... or ...; collect "err"
            | p::ps ->
                match getParser p inp state with
                | POk _ as res -> res
                | PError err -> iter ps
        iter parsers
