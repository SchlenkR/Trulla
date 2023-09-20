open System

type Parser<'value> = Parser of (ParserInput -> ParseResult<'value>)
and [<Struct>] PVal<'value> =
    {
        index: int
        value: 'value
    }
and ParserInput = PVal<string>
and ParseResult<'out> = 
    | POk of PVal<'out>
    | PError of ParseError list
and [<Struct>] ParseError =
    { 
        index: int
        message: string 
    }
and [<Struct>] DocPos =
    {
        index: int
        line: int
        column: int 
    }

module String =
    let equalsAt (index: int) (compareWith: string) (value: string) =
        index + compareWith.Length <= value.Length
        && value.Substring(index, compareWith.Length) = compareWith

module DocPos =
    
    // TODO: Perf: The parser combinators could track that, instead of computing it from scratch.
    let create (index: int) (input: string) =
        if index < 0 || index > input.Length then
            failwithf "Index %d is out of range of input string of length %d." index input.Length
        let lineStart,columnStart = 1,1
        let rec findLineAndColumn (currIdx: int) (line: int) (column: int) =
            if currIdx = index then
                { index = index; line = line; column = column }
            else
                let line,column =
                    if input |> String.equalsAt currIdx "\n"
                    then line + 1, columnStart
                    else line, column + 1
                findLineAndColumn (currIdx + 1) line column
        findLineAndColumn 0 lineStart columnStart

    let ofInput (pi: ParserInput) = create pi.index pi.value

module PError =
    let create inp message =
        PError [ { index = inp; message = message } ]

let inline mkParser parser = Parser parser
let inline getParser (Parser parser) = parser

let inline bind
    ([<InlineIfLambda>] f: 'a -> Parser<_>)
    (parser: Parser<_>) 
    =
    mkParser <| fun inp ->
        match (getParser parser) inp with
        | POk pRes ->
            let fParser = getParser (f pRes.value)
            let fResult = fParser { inp with index = pRes.index }
            fResult
        | PError errors -> PError errors

let return' value = 
    mkParser <| fun inp ->
        POk { index = inp.index; value = value }

let zero = return' ()

let combine (p1: Parser<_>) (p2: Parser<_>) =
    mkParser <| fun inp ->
        let p1Res = (getParser p1) inp
        match p1Res with
        | POk p1Res -> (getParser p2) { inp with index = p1Res.index }
        | _ -> p1Res

type [<Struct>] WhileGuard = { currIdx: int; errors: ParseError list }

let while' (guard: WhileGuard -> bool) body =
    mkParser <| fun inp ->
        let mutable currResults = []
        let mutable currIdx = inp.index
        let mutable currErrors =  []
        while guard { currIdx = currIdx; errors = currErrors } do
            let pBody = getParser (body ())
            match pBody inp with
            | PError errors ->
                do currErrors <- [ yield! currErrors; yield! errors ]
            | POk res ->
                do currResults <- res.value :: currResults
                do currIdx <- res.index
        match currErrors with
        | [] -> POk { index = inp.index; value = currResults }
        | errors -> PError errors

let inline run (value: string) (parser: Parser<_>) =
    let inp = { index = 0; value = value }
    (getParser parser) inp

let pmaybe defaultValue (p: Parser<_>) =
    mkParser <| fun inp ->
        match (getParser p) inp with
        | POk pRes -> POk pRes
        | PError _ -> POk { index = inp.index; value = defaultValue }

let pstr (s: string) =
    mkParser <| fun inp ->
        let res =
            inp.index + s.Length <= inp.value.Length 
            && inp.value.Substring(inp.index, s.Length) = s 
        match res with
        | true -> POk { index = inp.index + s.Length; value = s }
        | false -> PError.create inp.index $"Expected '{s}'."

let ( ~+ ) value = pstr value
let ( ~- ) value = pmaybe (pstr value)

//type All<'a> = All of 'a seq
type CanParse = CanParse

type ParserBuilder() =
    member inline _.Bind(p, [<InlineIfLambda>] f) = bind f p
    member _.Return(value) = return' value
    member _.ReturnFrom(value) = value
    member _.Yield(value) = return' value
    member _.YieldFrom(value) = value
    member _.Zero() = zero
    member _.Combine(p1, fp2) = combine p1 (fp2 ())
    member _.While(guard: unit -> bool, body) = while' (fun _ -> guard ()) body
    member _.Delay(f) = f
    member _.Run(f) = f ()
    member this.For(sequence: _ seq, body) =
        let enum = sequence.GetEnumerator()
        while' (fun _ -> enum.MoveNext()) (fun () -> body enum.Current)

let parse = ParserBuilder()

// Test while

parse {
    while true do
        yield ""
        //let x = pstr ""
        //let! x = x
        //yield x
}

//parse {
//    let x = WhileGuardWithState (fun x -> x.currIdx = 0)
//    while x do
//        let x = pstr ""
//        yield x
//}

// Test while

let map f (p: Parser<_>) =
    parse {
        let! x = p
        return f x
    }

let pignore (p: Parser<_>) =
    parse {
        let! _ = p
        return ()
    }

let pblank = pstr " "

//let puntil (untilParser: Parser<_>) =
//    parse {
//        let x = parse {
//        }
//    }

let pblanks =
    mkParser <| fun inp ->
        let mutable currIdx = inp.index
        while currIdx < inp.value.Length && inp.value.[currIdx] = ' ' do
            do currIdx <- currIdx + 1
        POk { index = currIdx; value = () }

parse {
    let! start = +"["
    let! a = parse {
        for x in 1..10 do
            let! x = +"x"
            yield x
        //while true do
        //    let! x = % "x"
        //    yield x
    }

    return "YES: start"
}
|> run " {{xxx"
