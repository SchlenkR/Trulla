module Trulla.Internal.CodeGen.FSharp

open Trulla.Internal
open Parsing
open Typing

let rootIdentifier = "model"
let dotIntoMember = "."

let makeTypeName tvar = match tvar with Root -> "TRoot" | TVar tvar -> $"T{tvar}"

// TODO: Make that configurable
let rec toTypeName typ =
    match typ with
    | Mono KnownTypes.string -> "string"
    | Mono KnownTypes.bool -> "bool"
    | Poly (KnownTypes.sequence, pt) -> $"list<{toTypeName pt}>"
    | Record tvar -> makeTypeName tvar
    | _ -> failwith $"Unsupported reference for type '{typ}'."

let rec memberExpToIdent (exp: TVal<MemberExp>) =
    match exp.value with
    | IdentExp ident ->
        let isBound = exp.bindingContext |> Map.containsKey ident
        let rootPrefix = if isBound then "" else rootIdentifier + dotIntoMember
        rootPrefix + ident
    | AccessExp acc -> (memberExpToIdent acc.instanceExp) + dotIntoMember + acc.memberName

let render (template: string) =
    let sb = System.Text.StringBuilder()
    let append (x: string) = sb.Append x |> ignore

    let quot = "\""
    let plus = " + "
    let pareno = "("
    let parenc = ")"
    let newLine = "\n"

    let getIndent indent = String.replicate indent "    "

    let linei indent x = append ((getIndent indent) + x + newLine)
    let line = linei 0
    let str x = quot + x + quot
    let strPlus x = append ((str x) + plus)
    let inParens x = append (pareno + x + parenc)
    
    let sb fn indent x = line $"""{getIndent indent}sb.{fn}("%s{x}")"""
    let sbAppend = sb "Append"
    let sbAppendLine = sb "AppendLine"
    
    parseTemplate template |> solve |> Result.map (fun (tree,records) ->
        line "namespace rec TODO" // TODO
        append newLine

        for tvar,fields in Map.toList records do
            line $"""type {makeTypeName tvar} = {{"""
            for (fn,ft) in fields do
                line $"    {fn}: {toTypeName ft}"
            line "}"
            append newLine
            
        append newLine
        line "let sb = System.Text.StringBuilder()"
        append newLine
                
        // TODO: Escape Quotes in strings
        line "let renderTemplate () ="
        let rec render indent tree =
            for texp in tree do
                match texp with
                | Text txt ->
                    sbAppend indent txt
                | Hole hole ->
                    sbAppend indent (memberExpToIdent hole)
                | For (ident,exp,body) ->
                    inParens ($"for {ident.value} in {memberExpToIdent exp} do")
                    render (indent+1) body
                | If (cond,body) ->
                    linei indent $"if {memberExpToIdent cond} then"
                    render (indent+1) body
        render 0 tree
        
        append newLine

        sb.ToString()
    )
