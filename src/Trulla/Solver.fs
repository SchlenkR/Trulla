[<RequireQualifiedAccess>]
module Trulla.Solver

open Trulla.Internal.Utils
open Trulla.Internal.Parsing
open Trulla.Internal.Ast
open Trulla.Internal.Inference

type SolveResult =
    {
        solution: Map<TVar, Typ>
        tree: TExp list
        records: RecordDef list
    }

let [<Literal>] RootRecordName = "Root"

let solveParseResult (parserResult: ParseResult) =
    result {
        let! tokens = parserResult 
        let! tree = Ast.buildTree tokens
        let problems = Inference.buildProblems tree
        let! solution = Inference.solveProblems problems.problems
        let getRecordName =
            // TODO: How we know that we have at least one?
            // TODO: Pascal case names / general: name checks all over the place
            let map = 
                problems.possibleRecordNames
                |> List.groupBy fst
                |> Map.ofList
                |> Map.map (fun _ v -> v |> List.map snd)
            fun recId ->
                // why match? Should be indifferent
                match recId with
                | Root -> RootRecordName
                | TVar _ -> map |> Map.find recId |> List.head
        let makeRecord tvar fields =
            {
                id = tvar
                name = getRecordName tvar
                fields = fields |> List.map snd
            }
        let records =
            solution
            |> List.choose (fun (tvar,t) ->
                match t with
                | Field f -> Some (tvar,f)
                | _ -> None)
            |> List.groupBy fst
            |> List.map (fun (tvar, fields) -> makeRecord tvar fields)
            |>
                function
                | [] -> [ makeRecord Root [] ]
                | records -> records

        return { 
            solution = solution |> Map.ofList
            tree = tree
            records = records 
        }
    }

let solve template =
    Parsing.parseTemplate template |> solveParseResult
