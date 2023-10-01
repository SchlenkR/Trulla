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
    { idx: int
      result: 'out }

and [<Struct>] ParseError =
    { idx: int
      message: string }

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

type [<Struct>] DocPos =
    { idx: int
      ln: int
      col: int }

type ForState<'a,'b>(id: 'a, appendItem: 'a -> 'a -> 'a) =
    member val ShallStop = false with get, set
    member _.Id = id
    member _.AppendItem curr item = appendItem curr item

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
            fParser (inp.Goto(pRes.idx)) state

let preturn value =
    mkParser <| fun inp state -> 
        POk { idx = inp.idx; result = value }

let hasRemainingChars<'s> n =
    fun (inp: Cursor) (state: 's) ->
        if not (inp.CanWalkFwd n)
        then PError { idx = inp.idx; message = sprintf "Expected %d more characters." n }
        else POk { idx = inp.idx; result = () }

let notAtEnd parserFunc = hasRemainingChars 1 parserFunc

// assert idx didn't move backwards
let hasConsumed lastIdx currIdx = lastIdx > currIdx
let standsStill lastIdx currIdx = lastIdx = currIdx

let pwhen pred p =
    mkParser <| fun inp state ->
        match pred inp state with
        | PError err -> PError { idx = inp.idx; message = err.message }
        | POk _ -> getParser p inp state

let mkParserWhen pred pf = pwhen pred <| mkParser pf

let pseq (s: _ seq) =
    let enum = s.GetEnumerator()
    mkParser <| fun inp state ->
        let mn = enum.MoveNext()
        if mn
        then POk { idx = inp.idx; result = enum.Current }
        else PError { idx = inp.idx; message = "No more elements in sequence." }

let inline run (text: string) (parser: Parser<_,_>) =
    let state = ForState.createForNothing()
    match getParser parser { idx = 0; original = text} state with
    | POk res -> Ok res.result
    | PError error ->
        let docPos = DocPos.create error.idx text
        Error {| pos = docPos; message = error.message |}

type Break = Break

type ParserBuilderBase() =
    member inline _.Bind(p, f) = bind f p
    member _.Return(x) = preturn x
    member _.ReturnFrom(p: Parser<_,_>) = p
    member _.YieldFrom(Break _) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            do state.ShallStop <- true
            POk { idx = inp.idx; result = state.Id }
    member _.Zero() =
        mkParser <| fun inp (state: ForState<_,_>) ->
            POk { idx = inp.idx; result = state.Id }
    member _.Combine(p1, fp2) = 
        mkParser <| fun inp (state: ForState<_,_>) ->
            let p2 = fp2 ()
            match getParser p1 inp (state: ForState<_,_>) with
            | POk p1Res ->
                match getParser p2 (inp.Goto p1Res.idx) state with
                | POk p2Res ->
                    let res = state.AppendItem p1Res.result p2Res.result
                    POk { idx = p2Res.idx; result = res }
                | PError error -> PError error
            | PError error -> PError error
    member _.For(loopParser, body) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            let rec iter currIdx currResult =
                match getParser loopParser (inp.Goto currIdx) state with
                | PError err -> printfn "ERR-LOOP"; POk { idx = currIdx; result = currResult }
                | POk loopRes ->
                    let bodyP = body loopRes.result
                    match getParser bodyP (inp.Goto loopRes.idx) state with
                    | PError err -> PError err
                    | POk bodyRes ->
                        let currResult = state.AppendItem currResult bodyRes.result
                        match state.ShallStop with
                        | true -> POk { idx = bodyRes.idx; result = currResult }
                        | false -> iter bodyRes.idx currResult
                            // if standsStill bodyRes.idx currIdx
                            // then PError { idx = bodyRes.idx; message = "Loop didn't advance." }
                            // else iter bodyRes.idx currResult
            iter inp.idx state.Id
    member this.For(sequence: _ seq, body) =
        this.For(pseq sequence, body)
    member this.While(guard, body) =
        let sequence = seq { while guard () do yield () }
        this.For(pseq sequence, body)
    member _.Delay(f) = f

type ParserBuilderForStringAppend() =
    inherit ParserBuilderBase()
    member _.Yield(x) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            POk { idx = inp.idx; result = x }
    member _.YieldFrom(p: Parser<_,_>) = p
    member _.Run(f) =
        mkParser <| fun inp state ->
            let state = ForState.createForStringAppend()
            getParser (f ()) inp state

type ParserBuilderForListAppend() =
    inherit ParserBuilderBase()
    member _.Yield(x) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            POk { idx = inp.idx; result = [x] }
    member _.YieldFrom(p: Parser<_,_>) =
        mkParser <| fun inp (state: ForState<_,_>) ->
            match getParser p inp state with
            | POk res -> POk { idx = res.idx; result = [res.result] }
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
        | POk pRes -> POk { idx = pRes.idx; result = proj pRes.result }

let pignore p =
    p |> map (fun _ -> ())

let pattempt p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk { idx = res.idx; result = Some res }
        | PError err -> PError err

let pok p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk { idx = res.idx; result = Some res }
        | PError err -> POk { idx = inp.idx; result = None }

let perr p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk { idx = res.idx; result = None }
        | PError err -> POk { idx = inp.idx; result = Some err }

let pisOk p = pok p |> map (fun x -> Option.isSome x)

let pisErr p = pok p |> map (fun x -> Option.isSome x)

let pnot p =
    mkParser <| fun inp state ->
        match getParser (pattempt p) inp state with
        | POk _ -> PError { idx = inp.idx; message = "Unexpected." }
        | PError _ -> POk { idx = inp.idx; result = () }

let punit<'s> =
    mkParser <| fun inp (state: 's) ->
        POk { idx = inp.idx; result = () }

let pstr<'s> (s: string) =
    mkParser <| fun inp (state: 's) ->
        if inp.StartsWith(s)
        then POk { idx = inp.idx + s.Length; result = s }
        else PError { idx = inp.idx; message = sprintf "Expected: '%s'" s }
let ( ~% ) = pstr

let pgoto (idx: int) =
    mkParser <| fun inp state ->
        if inp.CanGoto(idx) then 
            POk { idx = idx; result = () }
        else
            // TODO: this propably would be a fatal, most propably an unexpected error
            let msg = sprintf "Index %d is out of range of string of length %d." idx inp.original.Length
            PError { idx = idx; message = msg }

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
            match getParser b (inp.Goto ares.idx) state with
            | POk bres -> POk { idx = bres.idx; result = (ares.result, bres.result) } 
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
        POk { idx = inp.idx + 1; result = inp.Rest.[0].ToString() }

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
            | POk _ -> POk { idx = currIdx; result = inp.original.Substring(inp.idx, currIdx - inp.idx) }
            | PError _ ->
                if not (inp.CanGoto(currIdx + 1))
                then PError { idx = currIdx; message = "End of input." }
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
        then POk { idx = inp.idx + 1; result = string c }
        else PError { idx = inp.idx; message = errMsg c }

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
