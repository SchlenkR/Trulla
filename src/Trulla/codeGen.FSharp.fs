module Trulla.Internal.CodeGen.FSharp

open System.IO

open Trulla.Internal.Utils.Text
open Trulla.Internal.Parsing
open Trulla.Internal.Typing
open System

let rootIdentifier = "model"
let dotIntoMember = "."

let makeTypeName (possibleRecordNames: (TVar * string) list) tvar =
    possibleRecordNames
    |> List.tryFind (fun x -> fst x = tvar)
    |> Option.map snd
    |> Option.map (fun name ->
        match name.ToCharArray() |> Array.toList with
        | c::cs -> Char.ToUpperInvariant c :: cs
        | [] -> failwith "Empty possible record name is not supported."
        |> List.toArray
        |> String
    )
    |> Option.defaultWith (fun () ->
        match tvar with
        | Root -> "Root"
        | TVar tvar -> $"T{tvar}"
    )

// TODO: Make that configurable
let rec toTypeName possibleRecordNames typ =
    match typ with
    | Mono KnownTypes.string -> "string"
    | Mono KnownTypes.bool -> "bool"
    | Poly (KnownTypes.sequence, pt) -> $"list<{toTypeName possibleRecordNames pt}>"
    | Record tvar -> makeTypeName possibleRecordNames tvar
    | Var _ -> "obj"
    | _ -> failwith $"Unsupported reference for type '{typ}'."

let rec memberExpToIdent (exp: TVal<MemberExp>) =
    match exp.value with
    | IdentExp ident ->
        let isBound = exp.bindingContext |> Map.containsKey ident
        let rootPrefix = if isBound then "" else rootIdentifier + dotIntoMember
        rootPrefix + ident
    | AccessExp acc -> (memberExpToIdent acc.instanceExp) + dotIntoMember + acc.memberName

let textHelperFileContent =
    // we use the same text API inside of our generated file as we use also in this project
    let beginContentSignal = "// #begin"
    text {
        ln "[<AutoOpen>]"
        ln "module internal Text ="
        
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Utils/text.fs"))
        |> Seq.skipWhile (fun line -> not (line.StartsWith beginContentSignal))
        |> Seq.skip 1
        |> Seq.map (ind 1)
    }

let render (template: string) =
    parseTemplate template |> solve |> Result.map (fun solveResult -> text {
        ln "#if INTERACTIVE"
        ln "#else"
        ln "namespace TODO" // TODO
        ln "#endif"
        br

        ln textHelperFileContent
        br

        ln "[<AutoOpen>]"
        ln "module rec ModelTypes ="
        br

        // render records
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
        br
                    
        // TODO: Escape Quotes in strings
        ln "module Template ="
        lni 1 "open ModelTypes"
        br

        lni 1 $"let render ({rootIdentifier}: {makeTypeName solveResult.possibleRecordNames Root}) ="
        let rec render indent tree = text {
            lni indent "text {"
            let indent = indent + 1
            for texp in tree do
                match texp with
                | Text txt ->
                    strlni indent txt
                | Hole hole ->
                    lni indent (memberExpToIdent hole)
                | For (ident,exp,body) ->
                    lni indent $"for %s{ident.value} in {memberExpToIdent exp} do"
                    render (indent+1) body
                | If (cond,body) ->
                    lni indent $"if {memberExpToIdent cond} then"
                    render (indent+1) body
            lni indent "}"
        }

        render 2 solveResult.tree
    }
)

