module Trulla.Internal.CodeGen.Common

open System
open Trulla.Internal.Utils
open Trulla.Internal.ModelInference

let [<Literal>] rootIdentifier = "model"
let [<Literal>] dotIntoMember = "."

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
