
fsi.PrintWidth <- 120
fsi.PrintLength <- 150

#r "nuget: FParsec, 1.1.1"

#load "../parsing.fs"
#load "../typing.fs"
open Trulla.Parsing
open Trulla.Typing


let range number =
    let pos number = { index = number; line = 0; column = 0 }
    { start = pos number; finish = pos number }
let pval number t =
    { value = t; range = range number }
let accessExp number segments =
    pval number (Exp.createFromSegments segments)
let leaf tokenValue = LeafNode tokenValue
let node tokenValue children =
    InternalNode (tokenValue, children)
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()
let newGen() =
    let mutable x = -1
    let newNum() = x <- x + 1; x
    let toAcc (path: string) =
        let segments = path.Split [|'.'|] |> Array.toList
        accessExp (newNum()) segments
    let for' ident path = ParserToken.For (pval (newNum()) ident, toAcc path)
    let if' path = ParserToken.If (toAcc path)
    let hole path = ParserToken.Hole (toAcc path)
    let end' = End
    {| for' = for'; if' = if'; hole = hole; end' = end' |}
let constr x =
    let gen = newGen()
    x gen
    |> buildTree
    |> collectConstraints



let indentWith i = String.replicate (i * 4) " "
let printList o c indent singleLine l =
    let indent = indentWith indent
    let l = l |> List.map (sprintf "%A")
    if singleLine 
        then l |> String.concat "; " |> fun x -> $"{indent}{o} {x} {c}"
        else l |> List.map (fun x -> $"{indent}    {x}") |> String.concat $"\n" |> fun x -> $"{indent}{o}\n{x}\n{indent}{c}"
let rec print (o: obj) =
    match o with
    | :? Position as pos -> $"({pos.index})"
    | :? Range as range -> $"({print range.start}-{print range.finish})"
    | :? (Problem list) as x ->
        x
        |> List.map (fun (Problem (cl, cr)) ->
            $"%O{cl} : %O{cr}")
        |> printList "[" "]" 0 false
    | _ -> o.ToString()

fsi.AddPrinter <| fun (x: Position) -> $"({x.index})"
fsi.AddPrinter <| fun (x: Range) -> $"({print x.start}-{print x.finish})"
fsi.AddPrinter <| fun (x: Problem list) -> print x
//fsi.AddPrinter <| fun (x: (TVar * Type) list) ->
//    x
//    |> List.map (fun (tvar,typ) -> $"{print tvar} =\n{printi 2 typ}")
//    |> printList "[" "]" 0 false
