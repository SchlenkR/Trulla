module Trulla.Internal.CodeGen.FSharp

open System
open Trulla.Internal.Utils
open Trulla.Internal.Parsing
open Trulla.Internal.ModelInference
open Trulla.Internal.CodeGen.Common

let render (template: string) =
    let renderRecords (solveResult: SolveResult) = text {
        let records =
            if solveResult.records |> Map.containsKey Root
            then solveResult.records
            else solveResult.records |> Map.add Root []
        for tvar,fields in Map.toList records do
            lni 1 $"""type {makeTypeName solveResult.possibleRecordNames tvar} = {{"""
            for (fn,ft) in fields do
                lni 2 $"{fn}: {toTypeName solveResult.possibleRecordNames ft}"
            lni 1 "}"
            br
    }

    parseTemplate template |> solve |> Result.map (fun solveResult -> text {
        ln "#if INTERACTIVE"
        ln "#else"
        ln "namespace TODO" // TODO
        ln "#endif"
        br

        ln "[<AutoOpen>]"
        ln "module rec ModelTypes ="
        br

        // render records
        renderRecords solveResult
        br
                    
        // TODO: Escape Quotes in strings
        ln "module Template ="
        lni 1 "open System"
        lni 1 "open ModelTypes"
        br

        lni 1 $"let render ({rootIdentifier}: {makeTypeName solveResult.possibleRecordNames Root}) ="
        let sbAppend indent txt = text {
                ind indent $"""("%s{txt}" """
                ln "|> __sb.Append |> ignore)"
                br
            }
        lni 2 "let __sb = System.Text.StringBuilder()"
        let rec render indent tree = text {
            for texp in tree do
                match texp with
                | Text txt ->
                    sbAppend indent txt
                | Hole hole ->
                    sbAppend indent (memberExpToIdent hole)
                | For (ident,exp,body) ->
                    lni indent $"for %s{ident.value} in {memberExpToIdent exp} do"
                    render (indent + 1) body
                | If (cond,body) ->
                    lni indent $"if {memberExpToIdent cond} then"
                    render (indent + 1) body
        }

        render 2 solveResult.tree
    }
)

