module Trulla.NodeCli.Renderer

open System
open TheBlunt
open Trulla.Core
open Trulla.Core.Utils
open Trulla.Core.Text

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
    | Mono KnownTypes.bool -> "boolean"
    | Poly (KnownTypes.sequence, pt) -> $"Array<{toTypeName potentialRecordNames pt}>"
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
    let doubleQuotLit = "`"

    let toStringLiteral (txt: string) =
        // TODO: Escape that let txt = Microsoft.CodeAnalysis.CSharp.SymbolDisplay.FormatLiteral(txt, false)
        let txt = txt
        doubleQuotLit + txt + doubleQuotLit

    let newVar =
        let mutable i = 0
        fun () -> i <- i + 1; i

    text {
        // render records
        let records = solution.records
            // TODO: Why this?
            //if solution.records |> Map.containsKey Root
            //then solution.records
            //else solution.records |> Map.add Root []
        for r in records do
            let indent = 0
            lni indent $"type {makeTypeName records r.id} = {{"
            for field in r.fields do
                lni (indent + 1) $"readonly {field.name} : {toTypeName records field.typ}"
            lni indent "}"
            br

        lni 0 $"export function render ({rootIdentifier}: {makeTypeName solution.records Root}) {{"

        let sbAppend indent (txt: string) = text {
            lni indent $"""__s += {txt};"""
        }
        lni 1 "let __s = '';"

        let rec render indent tree = text {
            for texp in tree do
                match texp with
                | Text txt ->
                    sbAppend indent (toStringLiteral txt)
                | Hole hole ->
                    sbAppend indent (memberExpToIdent hole)
                | For (ident,exp,sep,body) ->
                    let elems = memberExpToIdent exp
                    let varI = $"i_{newVar()}"
                    lni indent $"let {varI} = 0;"
                    lni indent $"for (const {ident.value} of {elems}) {{"
                    render (indent + 1) body
                    let sep = sep.result |> Option.defaultValue ""
                    lni (indent + 1) $"""if ({varI} < {elems}.Count - 1) {{"""
                    sbAppend (indent + 2) (toStringLiteral sep)
                    lni (indent + 1) "}"
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
        
        render 1 solution.tree
        br
        lni 1 "return __s;"
        lni 0 "}"
    }

let renderErrors(errors: TrullaError seq) =
    let errorList = [ for error in errors do error.ToString() ]
    $"""
Errors in Trulla template:

{String.concat "\n\n" errorList};
"""
