namespace Trulla.Core

open Trulla.Core
open Trulla.Core.Utils

type Typ =
    | Mono of string
    | Poly of name: string * typParam: Typ
    | Field of Field
    | Record of TVar
    | Var of TVar

// TODO: After solving, a transition should happen from Type to FinalTyp.
// The FinalTyp also needs "real" poly types to support free vars 
// (see TODO in TrullaProvider.fs / finalizeProvidedRecord)
//type FinalTyp =
//    | FMono of string
//    | FPoly of name: string * typParam: FinalTyp
//    | FField of Field
//    | FRecord of TVar

and Field = { name: string; typ: Typ }

type ProblemItem = TVar * Typ

type SolutionItem = TVar * Typ // TODO: should be FinalTyp?

type Problem =
    | Unsolved of ProblemItem
    | Solved of SolutionItem

module KnownTypes =
    // TODO: reserve these keywords + parser tests
    let [<Literal>] string = "string"
    let [<Literal>] bool = "bool"
    let [<Literal>] sequence = "sequence"
    let sequenceOf elemTypId = sequence, elemTypId

[<RequireQualifiedAccess>]
module Inference =
    
    type Unification =
        | Unified of Problem list
        | KeepOriginal
    
    // TODO: Prevent shadowing (why? maybe allow it...)
    let buildProblems (tree: TExp list) =
        let rec constrainMemberExp (membExp: TVal<MemberExp>) =
            match membExp.value with
            | IdentExp ident ->
                let tvarIdent = membExp.bindingContext |> Map.tryFind ident
                match tvarIdent with
                | Some tvarIdent -> [ Unsolved (tvarIdent, Var membExp.tvar) ]
                | None -> [ Unsolved (Root, Field { name = ident; typ = Var membExp.tvar }) ]
            | AccessExp accExp ->
                [
                    yield! constrainMemberExp accExp.instanceExp
                    yield
                        Unsolved (
                            accExp.instanceExp.tvar,
                            Field { name = accExp.memberName; typ = Var membExp.tvar }
                        )
                ]
    
        let rec constrainTree (tree: TExp list) =
            [ for tree in tree do
                match tree with
                | Text _ -> ()
                | Hole hole ->
                    let holeProblems = constrainMemberExp hole
                    yield! holeProblems
                    yield Unsolved (hole.tvar, Mono KnownTypes.string)
                | For (ident,source,sep,children) ->
                    let tvarIdent = ident.tvar
                    let sourceProblems = constrainMemberExp source
                    yield! sourceProblems
                    yield Unsolved (source.tvar, Poly (KnownTypes.sequenceOf (Var tvarIdent)))
                    // --->
                    yield! constrainTree children
                | If (cond,children) ->
                    let condProblems = constrainMemberExp cond
                    yield! condProblems
                    yield Unsolved (cond.tvar, Mono KnownTypes.bool)
                    // --->
                    yield! constrainTree children
                | Else _ -> ()
            ]

        constrainTree tree

    let solveProblems (problems: Problem list) (tvarToMemberExp: Map<TVar, MemberExp>) =
        let mutable x = Map.empty

        let rec subst tvarToReplace withTyp inTyp =
            ////printfn $"Substing: {tvarToReplace} in {inTyp}"
            let withTyp =
                match withTyp with
                | Field _ -> Record tvarToReplace
                | _ -> withTyp
            match inTyp with
            | Poly (name, inTyp) -> Poly (name, subst tvarToReplace withTyp inTyp)
            | Field { name = fn; typ = ft } -> Field { name = fn; typ = subst tvarToReplace withTyp ft }
            | Var tvar when tvar = tvarToReplace -> withTyp
            | Record tvarRec ->
                match withTyp with
                | Var tvar when tvarRec = tvar -> Record tvar
                | _ -> inTyp
            | Mono _
            | Var _ -> inTyp
    
        let rec unify t1 t2 =
            ////printfn $"Unifying: ({t1})  --  ({t2})"
            // TODO: Correct range mapping when constructing new problems
            match t1,t2 with
            | t1,t2 when t1 = t2 ->
                Unified []
            | Var tv1, Var tv2 ->
                Unified [ Unsolved (tv2, Var tv1) ] // TODO: Why does tv2,tv1 work, but not tv1,tv2?
            | Var tvar, t
            | t, Var tvar ->
                Unified [ Unsolved (tvar, t) ]
            | Poly (n1,pt1), Poly (n2,pt2) when n1 = n2 ->
                unify pt1 pt2
            | Record tvarRec, (Field _ as r)
            | (Field _ as r), Record tvarRec ->
                Unified [ Unsolved (tvarRec, r) ]
            | Field { name = fn1; typ = ft1 }, Field { name = fn2; typ = ft2 } ->
                if fn1 = fn2 
                    then unify ft1 ft2
                    else KeepOriginal
            | _ ->
                // TODO: Don't raise
                { 
                    ranges = [] // TODO
                    message = sprintf "TODO: Can't unitfy types %s and %s" (t1.ToString()) (t2.ToString())
                }
                |> TrullaException
                |> raise
        
        let substInProblems tvarToReplace withType (inProblems: ProblemItem list) newProblem =
            ////printfn "RUN substInProblems ..."
            [ for ptvar, ptype in inProblems do
                let ptype = subst tvarToReplace withType ptype
                if ptvar = tvarToReplace then 
                    match unify withType ptype with
                    | Unified unifiedProblems ->
                        yield! unifiedProblems
                    | KeepOriginal ->
                        yield newProblem (ptvar, ptype)
                else
                    yield newProblem (ptvar, ptype)
            ]
    
        let rec solve (problems: Problem list) =
            ////printfn "---------------------- SOLVE"
            let solutions,problems =
                problems |> List.partitionMap (
                    function 
                    | Solved x -> Choice1Of2 x
                    | Unsolved x -> Choice2Of2 x)
            match problems with
            | [] -> solutions
            | ((tvar, typ) as p) :: ps ->
                solve [
                    yield! substInProblems tvar typ ps Unsolved
                    yield Solved p
                    yield! substInProblems tvar typ solutions Solved
                ]

        try Ok (solve problems)
        with TrullaException err -> Error [err]
