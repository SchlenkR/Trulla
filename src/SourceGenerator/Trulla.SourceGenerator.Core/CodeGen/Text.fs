module Trulla.SourceGenerator.Text

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
    member inline _.YieldFrom (f: StringBuffer) =
        f
    member inline _.Combine (f,g) = fun (b: StringBuilder) ->
        f b; g b
    member inline _.Delay (f) = fun (b: StringBuilder) ->
        (f()) b
    member inline _.Zero () =
        ignore
    member inline _.For (xs: 'a seq, f: 'a -> StringBuffer) = fun (b: StringBuilder) ->
        use e = xs.GetEnumerator ()
        while e.MoveNext() do
            (f e.Current) b
    member inline _.While (p: unit -> bool, f: StringBuffer) = fun (b: StringBuilder) ->
        while p () do f b
    member inline _.Run (f: StringBuffer) =
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
let ln0 = lni 0
let stri indent (txt: string) = ind indent (text { quot; txt; quot })
let str (txt: string) = stri 0 txt
let strlni indent (txt: string) = ln0 (stri indent txt)
let strln (txt: string) = ln0 (str txt)
