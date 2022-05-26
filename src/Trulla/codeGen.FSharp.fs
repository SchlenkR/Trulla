module Trulla.Internal.CodeGen.FSharp

open Trulla.Internal
open Helper
open Parsing
open Typing

let makeTypeName tvar = match tvar with Root -> "TRoot" | TVar tvar -> $"T{tvar}"

// TODO: Make that configurable
let rec toTypeName typ =
    match typ with
    | Mono KnownTypes.string -> "string"
    | Mono KnownTypes.bool -> "bool"
    | Poly (KnownTypes.sequence, pt) -> $"list<{toTypeName pt}>"
    | Record tvar -> makeTypeName tvar
    | _ -> failwith $"Unsupported reference for type '{typ}'."

let rec expToIdent (exp: MemberExp) ranges2tvar =
    match exp with
    | IdentExp ident ->
        let isRoot
        ident.value
    | AccessExp acc -> expToIdent acc.instanceExp.value + "." + acc.memberName

let render (template: string) =
    let newLine = "\n"
    let line x = x + newLine
    let quot = "\""
    let plus = " + "
    let text x = quot + x + quot
    let textLine x = line (text x)
    let textPlus x = quot + x + quot + plus
    let textLinePlus x = line (textPlus x)
    
    result {
        let! tree,records,ranges2tvar = parseTemplate template |> solve
        let lines = 
            [
                yield line "namespace rec TODO"
                for tvar,fields in Map.toList records do
                    let typeName = makeTypeName tvar
                    yield line $"""type {typeName} = {{"""
                    for (fn,ft) in fields do
                        yield line $"    {fn}: {toTypeName ft}"
                    yield line "}"
                    yield newLine
            
                for node in tree do
                    match node with
                    | LeafNode (Text txt) -> yield textPlus txt
                    | LeafNode (Hole exp) -> exp.value
            ]
            |> String.concat ""
        return lines
    }
