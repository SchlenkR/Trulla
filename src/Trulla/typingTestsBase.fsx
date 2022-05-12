
#r "nuget: FParsec"

#load "parsing.fs"
#load "typing.fs"
open Trulla
open Trulla.Parsing
open Trulla.Typing

let range number =
    let pos number = { index = number; line = 0; column = 0 }
    { start = pos number; finish = pos number }
let pval number t =
    { value = t; range = range number }
let accessExp number ident propPath =
    pval number { ident = ident; propPath = propPath }
let leaf tokenValue = LeafNode tokenValue
let node tokenValue children =
    InternalNode (tokenValue, children)
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()

fsi.AddPrinter(fun (p: Position) -> $"({p.index})")
//fsi.AddPrintTransformer(fun x ->
//    if x = null then null else
//    let t = x.GetType()
//    if t.Name = "PVal`1" then
//        let value = t.GetProperty("value").GetValue(x)
//        let s = t.GetProperty("start").GetValue(x)
//        let f = t.GetProperty("finish").GetValue(x)
//        $"{value} ({s}..{f})"
//    else
//        null
//)
