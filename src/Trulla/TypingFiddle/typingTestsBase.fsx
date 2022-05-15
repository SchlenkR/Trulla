
#r "nuget: FParsec"

#load "../parsing.fs"
#load "../typing.fs"
open Trulla.Parsing
open Trulla.Typing

fsi.PrintWidth <- 100


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

let rec print (o: obj) =
    match o with
    | :? Position as pos -> $"({pos.index})"
    | :? Range as range -> $"[{print range.start}-{print range.finish}]"
    | :? TypeId as tid -> let (TypeId tid) = tid in tid |> String.concat "__"
    | :? ExprConstraint as x -> $"{print x.typeId} : {print x.constr}"
    | :? Type as typ ->
        match typ with
        | Mono x -> print x
        | Poly (name,tid) -> $"{name}<{print tid}>"
        | Record r ->
            let fields = 
                r.fields
                |> List.map (fun f -> $"{f.name}: {print f.typ}")
                |> String.concat "; "
            sprintf "{ %s }" fields
        | x -> string x
    | _ -> sprintf "%A" o

fsi.AddPrinter <| fun (x: Position) -> print x
fsi.AddPrinter <| fun (x: Range) -> print x
fsi.AddPrinter <| fun (x: TypeId) -> print x
fsi.AddPrinter <| fun (x: Type) -> print x
fsi.AddPrinter <| fun (x: ExprConstraint) -> print x
