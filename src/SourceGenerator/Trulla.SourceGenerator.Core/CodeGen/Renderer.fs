module Trulla.SourceGenerator.Renderer

open System
open Trulla.Core.Utils
open Trulla.Core.Ast
open Trulla.Core.Inference
open Trulla.Core.Solver
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
    | Poly (KnownTypes.sequence, pt) -> sprintf "list<%s>" (toTypeName potentialRecordNames pt)
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

let renderTemplate (solveResult: SolveResult) =
    let prozS = "%s"
    
    text {
        ln "#if INTERACTIVE"
        ln "#else"
        ln "namespace TODO" // TODO
        ln "#endif"
        br

        ln "[<AutoOpen>]"
        ln "module rec ModelTypes ="
        br

        // render records
        text {
            let records = solveResult.records
                // TODO: Why this?
                //if solveResult.records |> Map.containsKey Root
                //then solveResult.records
                //else solveResult.records |> Map.add Root []
            for r in records do
                lni 1 (sprintf "type %s = {" (makeTypeName records r.id))
                for field in r.fields do
                    lni 2 (sprintf "%s: %s" field.name (toTypeName records field.typ))
                lni 1 "}"
                br
        }
        br
                    
        // TODO: Escape Quotes in strings
        ln "module Template ="
        lni 1 "open System"
        lni 1 "open ModelTypes"
        br

        lni 1 (sprintf "let render (%s: %s) =" rootIdentifier (makeTypeName solveResult.records Root))
        let sbAppend indent txt = text {
                ind indent (sprintf """("%s%s" """ prozS txt)
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
                | For (ident,exp,sep,body) ->
                    lni indent (sprintf "for %s%s in %s do" prozS (ident.value) (memberExpToIdent exp))
                    render (indent + 1) body
                | If (cond,body) ->
                    lni indent (sprintf "if %s then" (memberExpToIdent cond))
                    render (indent + 1) body
                | Else (cond,body) ->
                    lni indent "else"
                    render (indent + 1) body
        }

        render 2 solveResult.tree
    }
