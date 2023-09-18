module Trulla.SourceGenerator.Renderer

open System
open Trulla.Core
open Trulla.Core.Utils
open Trulla.SourceGenerator.Text

let [<Literal>] rootIdentifier = "model"
let [<Literal>] dotIntoMember = "."

module MemberExp =
    let getLastSegment = function
        | AccessExp accExp -> accExp.memberName
        | IdentExp ident -> ident

let makeTypeName (potentialRecordNames: RecordDef list) tvar =
    potentialRecordNames
    |> List.find (fun x -> x.id = tvar)
    |> fun x ->
        match x.name.ToCharArray() |> Array.toList with
        | c::cs -> Char.ToUpperInvariant c :: cs
        | [] -> failwith "Empty possible record name is not supported."
        |> List.toArray
        |> String

// TODO: Make that configurable
let rec toTypeName potentialRecordNames typ =
    match typ with
    | Mono KnownTypes.string -> "string"
    | Mono KnownTypes.bool -> "bool"
    | Poly (KnownTypes.sequence, pt) -> $"List<{toTypeName potentialRecordNames pt}>"
    | Record tvar -> makeTypeName potentialRecordNames tvar
    // TODO: See comments in ModelInference / FinalTyp
    //| Var _ -> "obj"
    | _ -> failwithf "Unsupported reference for type '%A'." typ

let rec memberExpToIdent (exp: TVal<MemberExp>) =
    match exp.value with
    | IdentExp ident ->
        let isBound = exp.bindingContext |> Map.containsKey ident
        let rootPrefix = if isBound then "" else rootIdentifier + dotIntoMember
        rootPrefix + ident
    | AccessExp acc -> (memberExpToIdent acc.instanceExp) + dotIntoMember + acc.memberName

let renderTemplate (solution: Solution) =text {
    ln0 "namespace TODO;" // TODO
    br
        
    ln0 "using System;"
    ln0 "using System.Collections.Generic;"
    ln0 "using System.Linq;"
    br
        
    // render records
    let records = solution.records
        // TODO: Why this?
        //if solution.records |> Map.containsKey Root
        //then solution.records
        //else solution.records |> Map.add Root []
    for r in records do
        let indent = 0
        lni indent $"public record {makeTypeName records r.id} {{"
        for field in r.fields do
            lni (indent + 1) $"public required {toTypeName records field.typ} {field.name} {{ get; init; }}"
        lni indent "}"
        br

    lni 0 $"public static class Rendering {{"
    lni 1 $"public static string Render(this {makeTypeName solution.records Root} {rootIdentifier}) {{"

    let sbAppend indent (txt: string) = text {
        lni indent $"""__sb.Append({txt});"""
    }
    lni 2 "var __sb = new System.Text.StringBuilder();"

    let rec render indent tree = text {
        for texp in tree do
            match texp with
            | Text txt ->
                let txt = Microsoft.CodeAnalysis.CSharp.SymbolDisplay.FormatLiteral(txt, false)
                let doubleQuotLit = "\""
                let txt = doubleQuotLit + txt + doubleQuotLit
                sbAppend indent txt
            | Hole hole ->
                sbAppend indent (memberExpToIdent hole)
            | For (ident,exp,sep,body) ->
                lni indent $"foreach (var {ident.value} in {memberExpToIdent exp}) {{"
                render (indent + 1) body
                lni indent "}"
            | If (cond,body) ->
                lni indent $"if ({memberExpToIdent cond}) {{"
                render (indent + 1) body
                lni indent "}"
            | Else (cond,body) ->
                lni indent "else {"
                render (indent + 1) body
                lni indent "}"
    }

    //lni (indent + 2) "}"
        
    render 2 solution.tree
    br
    lni 2 "return __sb.ToString();"
        
    lni 1 "}"

    lni 0 "}"
}
