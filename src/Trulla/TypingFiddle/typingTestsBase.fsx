
fsi.PrintWidth <- 120
fsi.PrintLength <- 150

#r "nuget: FParsec, 1.1.1"

#load "../parsing.fs"
#load "../typing.fs"
open Trulla.Internal.Parsing
open Trulla.Internal.Typing


let range number =
    let pos number = { index = number; line = 0; column = 0 }
    { start = pos number; finish = pos number }
let pval number t = { value = t; range = range number }
let accessExp segments = Exp.createFromSegments segments
let leaf tokenValue = LeafNode tokenValue
let node tokenValue children = InternalNode (tokenValue, children)
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()

type Gen() =
    let mutable x = -1
    let newNum() = x <- x + 1; x
    let toAcc (path: string) = accessExp [ for x in path.Split [|'.'|] do pval (newNum()) x ]
    member this.For ident path = ParserToken.For (pval (newNum()) ident, toAcc path) |> pval (newNum())
    member this.If path = ParserToken.If (toAcc path) |> pval (newNum())
    member this.Hole path = ParserToken.Hole (toAcc path) |> pval (newNum())
    member this.End = End |> pval (newNum())
let constr x =
    let gen = Gen()
    x gen
    |> buildTree
    |> Result.map collectConstraints
    


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
