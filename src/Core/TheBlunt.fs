
#if !INTERACTIVE
module TheBlunt
#endif

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


type ParserFunction<'value, 'state> = Cursor -> 'state -> ParserResult<'value>

and [<Struct>] Cursor(original: string, idx: int) =
    member _.Idx = idx
    member _.Original = original

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
module ParserHandling =
    #if INLINE_IF_LAMBDA

    type Parser<'value, 'state> = ParserFunction<'value, 'state>
    
    let inline mkParser (parser: Parser<_,_>) = parser
    let inline getParser (parser: Parser<_,_>) = parser

    module Inline =
        type IfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute

    #else

    type Parser<'value, 'state> = Parser of ParserFunction<'value, 'state>

    let inline mkParser parserFunction = Parser parserFunction
    let inline getParser (parser: Parser<_,_>) = let (Parser p) = parser in p

    module Inline =
        type IfLambdaAttribute() = inherit System.Attribute()

    #endif

type [<Struct>] DocPos =
    { idx: int
      ln: int
      col: int }

type ForState<'a>(id: 'a, append: 'a -> 'a -> 'a) =
    member val ShallStop = false with get, set
    member _.Id = id
    member _.Append curr item = append curr item

module ForState =
    let createForNothing () = ForState((), fun _ _ -> ())
    let createForStringAppend () = ForState("", fun curr item -> curr + item)

type Cursor with
    member c.CanGoto(idx: int) =
        // TODO: Should be: Only forward
        idx >= c.Idx && idx <= c.Original.Length
    member c.CanWalkFwd(steps: int) = c.CanGoto(c.Idx + steps)
    member c.IsAtEnd = c.Idx = c.Original.Length
    member c.HasRest = c.Idx < c.Original.Length
    member c.Rest : Str = c.Original.Slice(c.Idx)
    member c.StartsWith(s: string) = c.Rest.StringStartsWithAt(s, 0)
    member c.Goto(idx: int) =
        if not (c.CanGoto(idx)) then
            failwithf "Index %d is out of range of string of length %d." idx c.Original.Length
        Cursor(c.Original, idx)
    member c.WalkFwd(steps: int) = c.Goto(c.Idx + steps)
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
    let ofInput (pi: Cursor) = create pi.Idx pi.Original

// assert idx didn't move backwards
let hasConsumed lastIdx currIdx = lastIdx > currIdx
let standsStill lastIdx currIdx = lastIdx = currIdx

let inline bind (f: 'a -> Parser<_,_>) (parser: Parser<_,_>) =
    mkParser <| fun inp state ->
        match getParser parser inp state with
        | PError error -> PError error
        | POk pRes ->
            let fParser = getParser (f pRes.result)
            fParser (inp.Goto(pRes.idx)) state

let preturn value =
    mkParser <| fun inp state -> 
        POk { idx = inp.Idx; result = value }

let pseq (s: _ seq) =
    let enum = s.GetEnumerator()
    mkParser <| fun inp state ->
        if enum.MoveNext()
        then POk { idx = inp.Idx; result = enum.Current }
        else PError { idx = inp.Idx; message = "No more elements in sequence." }

let inline run (text: string) (parser: Parser<_,_>) =
    let state = ForState.createForNothing()
    match getParser parser (Cursor(text, 0)) state with
    | POk res -> Ok res.result
    | PError error ->
        let docPos = DocPos.create error.idx text
        Error {| pos = docPos; message = error.message |}

type Break = Break

type ParserBuilder() =
    member inline _.Bind(p, f) = bind f p
    member _.Return(x) = preturn x
    member _.ReturnFrom(p: Parser<_,_>) = p
    member _.Yield(x) =
        mkParser <| fun inp (state: ForState<_>) ->
            POk { idx = inp.Idx; result = x }
    member _.YieldFrom(Break _) =
        mkParser <| fun inp (state: ForState<_>) ->
            do state.ShallStop <- true
            POk { idx = inp.Idx; result = state.Id }
    member _.Zero() =
        mkParser <| fun inp (state: ForState<_>) ->
            POk { idx = inp.Idx; result = state.Id }
    member _.Combine(p1, fp2) = 
        mkParser <| fun inp (state: ForState<_>) ->
            let p2 = fp2 ()
            match getParser p1 inp (state: ForState<_>) with
            | POk p1Res ->
                match getParser p2 (inp.Goto p1Res.idx) state with
                | POk p2Res ->
                    let res = state.Append p1Res.result p2Res.result
                    POk { idx = p2Res.idx; result = res }
                | PError error -> PError error
            | PError error -> PError error
    member _.For(loopParser, body) =
        mkParser <| fun inp (state: ForState<_>) ->
            // TODO: This is hardcoced and specialized for Strings
            let rec iter currIdx currResult =
                match getParser loopParser (inp.Goto currIdx) state with
                | PError err -> POk { idx = currIdx; result = currResult }
                | POk loopRes ->
                    let bodyP = body loopRes.result
                    match getParser bodyP (inp.Goto loopRes.idx) state with
                    | PError err -> PError err
                    | POk bodyRes ->
                        let currResult = state.Append currResult bodyRes.result
                        let pok () = POk { idx = bodyRes.idx; result = currResult }
                        match state.ShallStop || inp.IsAtEnd with
                        | true -> pok ()
                        | false ->
                            if standsStill bodyRes.idx currIdx
                            then
                                let nextIdx = bodyRes.idx + 1
                                if not (inp.CanGoto(nextIdx))
                                then pok ()
                                else iter nextIdx currResult
                            else
                                iter bodyRes.idx currResult
            iter inp.Idx state.Id
    member this.For(sequence: _ seq, body) =
        this.For(pseq sequence, body)
        member _.Delay(f) = f
    member _.Run(f) =
        mkParser <| fun inp state ->
            let state = ForState.createForStringAppend()
            getParser (f ()) inp state


let parse = ParserBuilder()

// TODO: I guess that all the state stuff will turn out to be a quite unuseful,
// or compared to the 

let map proj (p: Parser<_,_>) =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | PError error -> PError error
        | POk pRes -> POk { idx = pRes.idx; result = proj pRes.result }

let pignore (p: Parser<_,_>) =
    p |> map (fun _ -> ())

let punit<'s> =
    mkParser <| fun inp (state: 's) ->
        POk { idx = inp.Idx; result = () }

let pstr<'s> (s: string) =
    mkParser <| fun inp (state: 's) ->
        if inp.StartsWith(s)
        then POk { idx = inp.Idx + s.Length; result = s }
        else PError { idx = inp.Idx; message = $"Expected: '{s}'" }

let goto (idx: int) =
    mkParser <| fun inp state ->
        if inp.CanGoto(idx) then 
            POk { idx = idx; result = () }
        else
            // TODO: this propably would be a fatal, most propably an unexpected error
            let msg = sprintf "Index %d is out of range of string of length %d." idx inp.Original.Length
            PError { idx = idx; message = msg }

let orThen a b =
    mkParser <| fun inp state ->
        match getParser a inp state with
        | POk res -> POk res
        | PError _ -> getParser b inp state
let ( <|> ) a b = orThen a b

let andThen a b =
    mkParser <| fun inp state ->
        match getParser a inp state with
        | POk ares ->
            match getParser b (inp.Goto ares.idx) state with
            | POk bres -> POk { idx = bres.idx; result = (ares.result, bres.result) } 
            | PError error -> PError error
        | PError error -> PError error
let ( <&> ) a b = andThen a b

let firstOf parsers = parsers |> List.reduce orThen

// TODO: sepBy
// TODO: skipN

//let puntil (until: Parser<_,_>) =
//    parse {
//    }

let anyChar<'s> =
    mkParser <| fun inp (state: 's) ->
        if inp.IsAtEnd
        then PError { idx = inp.Idx; message = "End of input." }
        else POk { idx = inp.Idx + 1; result = inp.Rest.[0].ToString() }

let eoi<'s> =
    mkParser <| fun inp (state: 's) ->
        if inp.Idx = inp.Original.Length - 1
        then POk { idx = inp.Idx + 1; result = () }
        else PError { idx = inp.Idx; message = "End of input." }

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

let attempt p =
    mkParser <| fun inp state ->
        match getParser p inp state with
        | POk res -> POk { idx = res.idx; result = Some res }
        | PError err -> PError err

let strUntil untilP p =
    mkParser <| fun inp state ->
        let rec iter currIdx =
            match getParser (attempt untilP) (inp.Goto currIdx) state with
            | POk _ -> POk { idx = currIdx; result = inp.Original.Substring(inp.Idx, currIdx - inp.Idx) }
            | PError _ ->
                if not (inp.CanGoto(currIdx + 1))
                then PError { idx = currIdx; message = "End of input." }
                else iter (currIdx + 1)
        iter inp.Idx

// let pSepByStr (p: Parser<_,_>) (sep: Parser<_,_>) =
//     parse {
//         for x
//     }

// TODO: passable reduce function / Zero for builder, etc.
// Name: Imparsible

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


let TEST () =

    blank |> run "   " |> Expect.ok " "
    blank |> run " "   |> Expect.ok " "
    blank |> run "x"   |> Expect.error
    blank |> run ""    |> Expect.error
    
    blanks 1 |> run "     xxx" |> Expect.ok "     "
    blanks 1 |> run "  xxx"    |> Expect.ok "  "
    blanks 1 |> run " xxx"     |> Expect.ok " "
    blanks 1 |> run "xxx"      |> Expect.error
    blanks 0 |> run "xxx"      |> Expect.ok ""

    parse {
        for x in anyChar do
            if x = "a" || x = "b" || x = "c" then
                yield x
            elif x = "X" then
                yield! Break
    }
    |> run "bacdeaXabb"
    |> Expect.ok "baca"

    ()
