
#r "nuget: FParsec, 1.1.1"

#load "../../Utils.fs"
#load "../../Parsing.fs"
#load "../../Ast.fs"
#load "../../Inference.fs"
#load "../../Solver.fs"
#load "./codeGenFSharp.fsx"

//module Trulla.Core.CodeGenFSharp

open System
open Trulla
open Trulla.Core.Utils
open Trulla.Core.Ast
open Trulla.Core.Inference

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
    | Poly (KnownTypes.sequence, pt) -> $"list<{toTypeName potentialRecordNames pt}>"
    | Record tvar -> makeTypeName potentialRecordNames tvar
    // TODO: See comments in ModelInference / FinalTyp
    //| Var _ -> "obj"
    | _ -> failwith $"Unsupported reference for type '{typ}'."

let rec memberExpToIdent (exp: TVal<MemberExp>) =
    match exp.value with
    | IdentExp ident ->
        let isBound = exp.bindingContext |> Map.containsKey ident
        let rootPrefix = if isBound then "" else rootIdentifier + dotIntoMember
        rootPrefix + ident
    | AccessExp acc -> (memberExpToIdent acc.instanceExp) + dotIntoMember + acc.memberName

let render (template: string) =
    let renderRecords (solveResult: Solver.SolveResult) = text {
        let records = solveResult.records
            // TODO: Why this?
            //if solveResult.records |> Map.containsKey Root
            //then solveResult.records
            //else solveResult.records |> Map.add Root []
        for r in records do
            lni 1 $"""type {makeTypeName records r.id} = {{"""
            for field in r.fields do
                lni 2 $"{field.name}: {toTypeName records field.typ}"
            lni 1 "}"
            br
    }

    Solver.solve template |> Result.map (fun solveResult -> text {
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

        lni 1 $"let render ({rootIdentifier}: {makeTypeName solveResult.records Root}) ="
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

