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
    | POk of ok: ParserResultValue<'out>
    | PError of error: ParseError

and [<Struct>] ParserResultValue<'out> =
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

type ForState<'a,'b>(id: 'a, appendItem: 'a -> 'a -> 'a) =
    member val ShallStop = false with get, set
    member _.Id = id
    member _.AppendItem curr item = appendItem curr item

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

module ForState =
    let createForNothing () = ForState((), fun _ _ -> ())
    let createForStringAppend () = ForState("", fun curr x -> curr + x)
    let createForListAppend () = ForState([], fun curr x -> curr @ x)

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

let inline bind (f: 'a -> Parser<_,_>) (parser: Parser<_,_>) =
    mkParser <| fun inp state ->
        match getParser parser inp state with
        | PError error -> PError error
        | POk pRes ->
            let fParser = getParser (f pRes.result)
            fParser (inp.Goto(pRes.range.endIdx)) state

let preturn value =
    mkParser <| fun inp state -> POk.create inp.idx inp.idx value

let hasRemainingChars<'s> n =
    fun (inp: Cursor) (state: 's) ->
        if not (inp.CanWalkFwd n)
        then PError.create inp.idx (sprintf "Expected %d more characters." n)
        else POk.create inp.idx inp.idx ()

let notAtEnd parserFunc = hasRemainingChars 1 parserFunc

// assert idx didn't move backwards
let hasConsumed lastIdx currIdx = lastIdx > currIdx
let standsStill lastIdx currIdx = lastIdx = currIdx

let pwhen pred p =
    mkParser <| fun inp state ->
        match pred inp state with
        | PError err -> PError.create inp.idx err.message
        | POk _ -> getParser p inp state

let mkParserWhen pred pf = pwhen pred <| mkParser pf

let pseq (s: _ seq) =
    let enum = s.GetEnumerator()
    mkParser <| fun inp state ->
        if enum.MoveNext()
        then POk.create inp.idx inp.idx enum.Current
        else PError.create inp.idx "No more elements in sequence."

let inline run (text: string) (parser: Parser<_,_>) =
    let state = ForState.createForNothing()
    match getParser parser { idx = 0; original = text} state with
    | POk res -> Ok res.result
    | PError error ->
        let docPos = DocPos.create error.idx text
        Error {| pos = docPos; message = error.message |}

type Break = Break

[<AbstractClass>]
type ParserBuilderBase() =
    member inline _.Bind(p, f) = bind f p
    member _.Return(x) = preturn x
    member _.ReturnFrom(p: Parser<_,_>) = p
    member _.YieldFrom(Break _) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            do state.ShallStop <- true
            POk.create inp.idx inp.idx state.Id
    member _.Zero() =
        mkParser <| fun inp (state: ForState<_,_>) ->
            POk.create inp.idx inp.idx state.Id
    member _.Combine(p1, fp2) = 
        mkParser <| fun inp (state: ForState<_,_>) ->
            let p2 = fp2 ()
            match getParser p1 inp (state: ForState<_,_>) with
            | POk p1Res ->
                match getParser p2 (inp.Goto p1Res.range.endIdx) state with
                | POk p2Res ->
                    let res = state.AppendItem p1Res.result p2Res.result
                    POk.createFromRange (Range.merge p1Res.range p2Res.range) res
                | PError error -> PError error
            | PError error -> PError error
    member _.For(loopParser, body) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            let rec iter currIdx currResult =
                match getParser loopParser (inp.Goto currIdx) state with
                | PError err -> POk.create inp.idx currIdx currResult
                | POk loopRes ->
                    let bodyP = body loopRes.result
                    match getParser bodyP (inp.Goto loopRes.range.endIdx) state with
                    | PError err -> PError err
                    | POk bodyRes ->
                        let currResult = state.AppendItem currResult bodyRes.result
                        match state.ShallStop with
                        | true -> POk.create inp.idx bodyRes.range.endIdx currResult
                        | false -> iter bodyRes.range.endIdx currResult
            iter inp.idx state.Id
    member this.For(sequence: _ seq, body) =
        this.For(pseq sequence, body)
    member this.While(guard, body) =
        let sequence = seq { while guard () do yield () }
        this.For(pseq sequence, body)
    member _.Delay(f) = f

[<Sealed>]
type ParserBuilderForStringAppend() =
    inherit ParserBuilderBase()
    member _.Yield(x) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            POk.create inp.idx inp.idx x
    member _.YieldFrom(p: Parser<_,_>) = p
    member _.Run(f) =
        mkParser <| fun inp state ->
            let state = ForState.createForStringAppend()
            getParser (f ()) inp state

[<Sealed>]
type ParserBuilderForListAppend() =
    inherit ParserBuilderBase()
    member _.Yield(x) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            POk.create inp.idx inp.idx [x]
    member _.YieldFrom(p: Parser<_,_>) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            match getParser p inp state with
            | POk res -> POk.createFromRange res.range [res.result]
            | PError err -> PError err
    member _.Run(f) =
        mkParser <| fun inp state ->
            let state = ForState.createForListAppend()
            getParser (f ()) inp state

let parse = ParserBuilderForStringAppend()
let parseItems = ParserBuilderForListAppend()

// TODO: I guess that all the state stuff will turn out to be a quite unuseful,
// or compared to the 

let map proj p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | PError error -> PError error
        | POk pRes -> POk.createFromRange pRes.range (proj pRes.result)

let mapRes proj p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | PError error -> PError error
        | POk pRes -> POk.createFromRange pRes.range (proj pRes)

let pignore p =
    p |> map (fun _ -> ())

let pattempt p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk.createFromRange res.range (Some res)
        | PError err -> PError err

let pok p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk.createFromRange res.range (Some res)
        | PError err -> POk.create inp.idx inp.idx None

let perr p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk.createFromRange res.range None
        | PError err -> POk.create inp.idx inp.idx (Some err)

let pisOk p = pok p |> map (fun x -> Option.isSome x)

let pisErr p = pok p |> map (fun x -> Option.isSome x)

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

let inline ( >+> ) a b = parse {
    let! pa = a
    let! pb = b
    pa + pb
}

let firstOf parsers = parsers |> List.reduce orThen

// TODO: sepBy
// TODO: skipN

let anyChar<'s> =
    mkParserWhen notAtEnd <| fun inp (state: 's) ->
        POk.create inp.idx (inp.idx + 1) (inp.Rest.[0].ToString())

// TODO: anyCharExcept(c,p)

let eoi<'s> : Parser<_,'s> =
    pwhen notAtEnd <| preturn ()

let blank<'s> = pstr<'s> " "

/// Parse at least n or more blanks.
let blanks n =
    parse {
        for x in 1 .. n do
            let! c = blank
            yield c
        for x in blank do
            yield x
    }

let untilStr untilP p =
    mkParser <| fun inp state ->
        let rec iter currIdx =
            match getParser (pattempt untilP) (inp.Goto currIdx) state with
            | POk _ -> POk.create inp.idx currIdx (inp.original.Substring(inp.idx, currIdx - inp.idx))
            | PError _ ->
                if not (inp.CanGoto(currIdx + 1))
                then PError.create currIdx "End of input."
                else iter (currIdx + 1)
        iter inp.idx

let manyStr (p: Parser<_,_>) =
    parse {
        for x in p do
            yield x
    }

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

let pchar<'s> pred errMsg =
    mkParserWhen notAtEnd <| fun inp (state: 's) ->
        let c = inp.Rest.[0]
        if pred c
        then POk.create inp.idx (inp.idx + 1) (string c)
        else PError.create inp.idx (errMsg c)

let letter<'s> =
    pchar<'s> (Char.IsLetter) (sprintf "Expected letter, but got '%c'.")

let digit<'s> =
    pchar<'s> (Char.IsDigit) (sprintf "Expected letter, but got '%c'.")

let pstrNotFollowedBy s suffix =
    parse {
        let! x = pstr s
        do! pnot (pstr suffix)
        return x
    }

let psepBy psep (pelem: Parser<_,_>) =
    parseItems {
        for x in pelem do
            yield x
            let! sepOk = pisOk psep
            if not sepOk then
                yield! Break
    }

let psepBy1 psep (pelem: Parser<_,_>) =
    parseItems {
        yield! pelem
        for x in psep >>. pelem do
            yield x
    }
