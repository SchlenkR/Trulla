module Trulla.Internal.CodeGen.FSharp

open Trulla.Internal
open Helper
open Parsing
open Typing

let recTypeName tvar = match tvar with Root -> "ROOT" | TVar tvar -> $"T{tvar}"

// TODO: Make that configurable
let rec toTypeName typ =
    match typ with
    | Mono KnownTypes.string -> "string"
    | Mono KnownTypes.bool -> "bool"
    | Poly (KnownTypes.sequence, pt) -> $"list<{toTypeName pt}>"
    | RecRef tvar -> recTypeName tvar
    | _ -> failwith $"Unsupported reference for type '{typ}'."

let render (template: string) =
    let newLine = "\n"
    let line x = x + newLine
    result {
        let! tree,records,ranges2tvar = parseTemplate template |> solve
        let lines = 
            [
                yield line "namespace rec TODO"
                for tvar,fields in Map.toList records do
                    let typeName = recTypeName tvar
                    yield line $"""type {typeName} = {{"""
                    for (fn,ft) in fields do
                        yield line $"    {fn}: {toTypeName ft}"
                    yield line "}"
                    yield newLine

                for node in tree do
                    match node with
                    | LeafNode (Text text) -> yield text
                    | LeafNode (Hole exp) -> 
            ]
            |> String.concat ""
        return lines
    }
