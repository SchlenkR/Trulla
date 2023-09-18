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

let renderTemplate (solution: Solution) =
    let prozS = "%s"
    
    text {
        ln "namespace TODO;" // TODO
        br

        // render records
        text {
            let records = solution.records
                // TODO: Why this?
                //if solution.records |> Map.containsKey Root
                //then solution.records
                //else solution.records |> Map.add Root []
            for r in records do
                lni 1 $"public record {makeTypeName records r.id} {{"
                for field in r.fields do
                    lni 2 $"public required {toTypeName records field.typ} {field.name} {{ get; init; }}"
                lni 1 "}"
                br
        }
        br
                    
        // TODO: Escape Quotes in strings
        lni 1 "using System;"
        lni 1 "using System.Collections.Generic;"
        lni 1 "using System.Linq;"
        br

        lni 1 $"public static class Rendering {{"
        lni 2 $"public static string Render({rootIdentifier}: {makeTypeName solution.records Root}) {{"
        let sbAppend indent (txt: string) = text {
            let txt = Microsoft.CodeAnalysis.CSharp.SymbolDisplay.FormatLiteral(txt, false)
            lni indent $"""__sb.Append(@"{txt}");"""
        }
        lni 3 "var __sb = new System.Text.StringBuilder();"
        let rec render indent tree = text {
            for texp in tree do
                match texp with
                | Text txt ->
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

        lni 2 "}"
        lni 1 "}"
        
        render 2 solution.tree
    }
