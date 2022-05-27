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
    | Var _ -> "obj"
    | _ -> failwith $"Unsupported reference for type '{typ}'."

let rec memberExpToIdent (exp: TVal<MemberExp>) =
    match exp.value with
    | IdentExp ident ->
        let isBound = exp.bindingContext |> Map.containsKey ident
        let rootPrefix = if isBound then "" else rootIdentifier + dotIntoMember
        rootPrefix + ident
    | AccessExp acc -> (memberExpToIdent acc.instanceExp) + dotIntoMember + acc.memberName

type StringBuilder2() =
    let sb = System.Text.StringBuilder()
    member this.Append(s: string) = sb.Append s |> ignore
    member this.AppendLine(s: string) = sb.AppendLine s |> ignore
    member this.NewLine() = sb.AppendLine ""
    member this.GetString() = sb.ToString()

let render (template: string) =
    let builder = StringBuilder2()
    let append (x: string) = builder.Append x |> ignore

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
    
    let sb fn indent x =
        let indent = getIndent indent
        line $"""{indent}sb.%s{fn}("%s{x}"{indent}) |> ignore"""
    let sbAppend = sb "Append"
    let sbAppendLine = sb "AppendLine"
    
    parseTemplate template |> solve |> Result.map (fun (tree,records) ->
        line "module rec TODO" // TODO
        append newLine

        let records = 
            if records |> Map.containsKey Root 
            then records
            else records |> Map.add Root []

        for tvar,fields in Map.toList records do
            line $"""type {makeTypeName tvar} = {{"""
            for (fn,ft) in fields do
                line $"    {fn}: {toTypeName ft}"
            line "}"
            append newLine
            
        append newLine
                
        // TODO: Escape Quotes in strings
        line $"let renderTemplate ({rootIdentifier}: {makeTypeName Root}) ="
        linei 1 "let sb = System.Text.StringBuilder()"
        append newLine
        
        let rec render indent tree =
            for texp in tree do
                match texp with
                | Text txt ->
                    sbAppend indent txt
                | Hole hole ->
                    sbAppend indent (memberExpToIdent hole)
                | For (ident,exp,body) -> ()
                    //inParens ($"for {ident.value} in {memberExpToIdent exp} do")
                    //render (indent+1) body
                | If (cond,body) ->
                    linei indent $"if {memberExpToIdent cond} then"
                    render (indent+1) body
        render 1 tree
        
        append newLine

        builder.ToString()
    )
