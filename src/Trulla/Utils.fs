module Trulla.Internal.Utils

[<AutoOpen>]
module Result =
    type ResultBuilder() =
        member _.Bind(m, f) = Result.bind f m
        member _.Return(value) = Ok value
    let result = ResultBuilder()

[<AutoOpen>]
module List =
    let partitionMap (mapping: 'a -> Choice<'b,'c>) (source: list<'a>) =
        let rec loop ((acc1, acc2) as acc) =
            function
            | [] -> acc
            | x::xs ->
                match mapping x with
                | Choice1Of2 x -> loop (x::acc1, acc2) xs
                | Choice2Of2 x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)

[<AutoOpen>]
module Reflection =
    open Microsoft.FSharp.Reflection

    /// Returns the case name of the object with union type 'a.
    let getUnionCaseName (x:'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with case, _ -> case.Name

[<AutoOpen>]
module Text =
    open System.Text

    type StringBuffer = StringBuilder -> unit

    // Base from:
    // http://fssnip.net/7WR
    // https://github.com/fsharp/fslang-suggestions/issues/775
    type TextBuilder () =
        member inline _.Yield (txt: string) = fun (b: StringBuilder) -> 
            Printf.bprintf b "%s" txt
        member inline _.Yield (c: char) = fun (b: StringBuilder) -> 
            Printf.bprintf b "%c" c
        member inline _.Yield (strings: #seq<string>) = fun (b: StringBuilder) ->
            for s in strings do Printf.bprintf b "%s\n" s
        member inline _.YieldFrom ([<InlineIfLambda>] f: StringBuffer) =
            f
        member inline _.Combine ([<InlineIfLambda>] f,[<InlineIfLambda>] g) = fun (b: StringBuilder) ->
            f b; g b
        member inline _.Delay ([<InlineIfLambda>] f) = fun (b: StringBuilder) ->
            (f()) b
        member inline _.Zero () =
            ignore
        member inline _.For (xs: 'a seq, [<InlineIfLambda>] f: 'a -> StringBuffer) = fun (b: StringBuilder) ->
            use e = xs.GetEnumerator ()
            while e.MoveNext() do
                (f e.Current) b
        member inline _.While ([<InlineIfLambda>] p: unit -> bool, [<InlineIfLambda>] f: StringBuffer) = fun (b: StringBuilder) ->
            while p () do f b
        member inline _.Run ([<InlineIfLambda>] f: StringBuffer) =
            let b = StringBuilder()
            do f b
            b.ToString()

    let text = TextBuilder()

    let quot = "\""
    let plus = " + "
    let pareno = "("
    let parenc = ")"
    let br = "\n"

    let getIndentation indent = String.replicate indent "    "
    let ind indent (txt: string) = text { getIndentation indent; txt }
    let lni indent (txt: string) = ind indent (text { txt; br })
    let ln = lni 0
    let stri indent (txt: string) = ind indent (text { quot; txt; quot })
    let str (txt: string) = stri 0 txt
    let strlni indent (txt: string) = ln (stri indent txt)
    let strln (txt: string) = ln (str txt)
