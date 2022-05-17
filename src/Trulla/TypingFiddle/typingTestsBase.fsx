
fsi.PrintWidth <- 170
fsi.PrintLength <- 150

#r "nuget: FParsec"

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
////let unify constraints =
////    let u = constraints |> unifyConstraints
////    let types = u |> List.choose (fun x ->
////        if x.errors.Length > 0 then None else Some (x.typeId,x.resultingTyp))
////    let errors =
////        u 
////        |> List.collect (fun x -> if x.errors.Length > 0 then x.errors else [])
////        |> List.map (fun e -> e.message)
////    types,errors


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
    | :? TypeId as tid ->
        let (TypeId tid) = tid
        match tid with
        | [] -> "$$ROOT$$"
        | tid -> tid |> String.concat "__"
    | :? (Problem list) as x ->
        x
        |> List.map (fun (Problem (TVar tvar, constr)) ->
            $"TVAR-{tvar} : {print constr}")
        |> printList "[" "]" 0 false
    | :? Type as typ ->
        match typ with
        | Mono x -> print x
        | Poly (name,tid) -> $"{name}<{print tid}>"
        | Record r ->
            r.fields
            |> List.map (fun f -> $"{f.name}: {print f.typ}")
            |> printList "{" "}" 1 false
        | x -> string x
    | _ -> sprintf "%A" o
let printi indent o =
    let indent = indentWith indent
    print o 
    |> fun s -> s.Split [|'\n'|] 
    |> Array.map (fun x -> $"{indent}{x}")
    |> String.concat "\n"

fsi.AddPrinter <| fun (x: Position) -> print x
fsi.AddPrinter <| fun (x: Range) -> print x
fsi.AddPrinter <| fun (x: TypeId) -> print x
fsi.AddPrinter <| fun (x: Type) -> print x
fsi.AddPrinter <| fun (x: Problem list) -> print x
fsi.AddPrinter <| fun (x: (TypeId * Type) list) ->
    x
    |> List.map (fun (tid,typ) -> $"{print tid} =\n{printi 1 typ}")
    |> printList "[" "]" 0 false
