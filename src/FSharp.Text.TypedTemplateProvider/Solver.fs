namespace Trulla.Core.Solver

open Trulla.Core.Utils
open Trulla.Core.Parsing
open Trulla.Core.Ast
open Trulla.Core.Inference

type RecordDef =
    {
        id: TVar
        fields: Field list
        name: string
    }

type SolveResult =
    {
        solution: Map<TVar, Typ>
        tree: TExp list
        records: RecordDef list
    }

[<RequireQualifiedAccess>]
module Solver =
    let [<Literal>] RootRecordName = "Root"

    let solveParseResult (parserResult: ParseResult) =
        result {
            let mutable usedRecordNames = []

            let! tokens = parserResult 
            let! ast = Ast.buildTree tokens
            let problems = Inference.buildProblems ast.tree
            let! solution = Inference.solveProblems problems ast.tvarToMemberExp
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

            return
                {
                    solution = solution |> Map.ofList
                    tree = ast.tree
                    records = records 
                }
        }

    let solve template =
        Parsing.parseTemplate template |> solveParseResult
