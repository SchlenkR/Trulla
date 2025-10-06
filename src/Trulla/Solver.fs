namespace Trulla.Core

open TheBlunt
open Trulla.Core.Utils

type RecordDef =
    {
        id: TVar
        fields: Field list
        name: string
    }

type Solution =
    {
        solution: Map<TVar, Typ>
        tree: TExp list
        records: RecordDef list
    }

[<RequireQualifiedAccess>]
module Solver =
    let [<Literal>] RootRecordName = "Root"

    let solveParseResult parserResult =
        let mutable usedRecordNames = []

        match parserResult with
        | PError err -> Error [ { range = Range.create err.idx err.idx; message = err.message } ]
        | POk tokens  ->
            match Ast.buildTree tokens.result with
            | Error errs -> Error errs
            | Ok ast ->
                let problems = Inference.buildProblems ast.tree
                match Inference.solveProblems problems ast.tvarToMemberExp with
                | Error errs -> Error errs
                | Ok solution ->
                    let makeRecord tvar fields =
                        let inferedRecordName =
                            match tvar with
                            | Root -> RootRecordName
                            | TVar tvar ->
                                let lastSegmentOfMemberExp =
                                    let memberExp = ast.tvarToMemberExp |> Map.find (TVar tvar) fields
                                    match memberExp with
                                    | IdentExp ident -> ident
                                    | AccessExp accExp -> accExp.memberName
                                let count = 
                                    usedRecordNames 
                                    |> List.filter (fun x -> x = lastSegmentOfMemberExp)
                                    |> List.length
                                do usedRecordNames <- lastSegmentOfMemberExp :: usedRecordNames
                                match count with
                                | 0 -> lastSegmentOfMemberExp
                                | n -> lastSegmentOfMemberExp + (n.ToString())
                        {
                            id = tvar
                            name = inferedRecordName
                            fields = fields
                        }
                    let records =
                        solution
                        |> List.choose (fun (tvar,t) ->
                            match t with
                            | Field f -> Some (tvar,f)
                            | _ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (tvar, fields) -> 
                            fields
                            |> List.map snd 
                            |> List.sortBy (fun f -> f.name)
                            |> makeRecord tvar
                        )
                        |>
                            function
                            | [] -> [ makeRecord Root [] ]
                            | records -> records
                    {
                        solution = solution |> Map.ofList
                        tree = ast.tree
                        records = records 
                    }
                    |> Ok

    let solve template =
        let parseResult = Parsing.parseTemplate template
        solveParseResult parseResult
