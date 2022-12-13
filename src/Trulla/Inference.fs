module Trulla.Internal.Inference

open Trulla.Internal.Utils
open Trulla.Internal.Parsing

type TVar =
    | Root
    | TVar of int

type BindingContext = Map<string, TVar>

type TVal<'a> =
    { range: Range
      tvar: TVar
      bindingContext: BindingContext
      value: 'a }
        override this.ToString() = $"({this.range}){this.value}"

type Body = BindingContext * TExp list
and TExp =
    | Text of string
    | Hole of TVal<MemberExp>
    | For of ident: TVal<string> * exp: TVal<MemberExp> * body: TExp list
    | If of cond: TVal<MemberExp> * body: TExp list
and MemberExp =
    | AccessExp of {| instanceExp: TVal<MemberExp>; memberName: string |}
    | IdentExp of string

type TVarGen() =
    let mutable x = -1
    member _.Next() =
        x <- x + 1
        ////printfn $"TVAR {x} %s{name}"
        TVar x

module TVal =
    let create range tvar bindingContext value =
        { range = range; tvar = tvar; bindingContext = bindingContext; value = value }

module MemberExp =
    let getLastSegment = function
        | AccessExp accExp -> accExp.memberName
        | IdentExp ident -> ident

// TODO: meaningful error messages + location
// TODO: Don't throw; return TemplateError results
let buildTree (tokens: PVal<Token> list) =
    let tvargen = TVarGen()

    let buildMemberExp bindingContext pexp =
        let rec ofPExpZero (pexp: PVal<MemberToken>) =
            let newTVal value = TVal.create pexp.range (tvargen.Next()) bindingContext value
            match pexp.value with
            | AccessToken accExp ->
                let accExp = {| instanceExp = ofPExpZero accExp.instanceExp; memberName = accExp.memberName |}
                newTVal (AccessExp accExp)
            | IdentToken ident ->
                newTVal (IdentExp ident)
        ofPExpZero pexp

    let rec toTree (pointer: int) scopeDepth (bindingContext: BindingContext) =
        // TODO: IS all that really necessary - the mutable stuff?
        let mutable pointer = pointer
        let mutable scopeDepth = scopeDepth
        let mutable endTokenDetected = false
        let tree =
            [ while not endTokenDetected && pointer < tokens.Length do
                let token = tokens[pointer]
                pointer <- pointer + 1

                let processBody bindingContext : TExp list =
                    let newPointer,children,newScopeDepth = toTree pointer (scopeDepth + 1) bindingContext
                    scopeDepth <- newScopeDepth
                    pointer <- newPointer
                    children

                match token.value with
                | Token.Text x -> Text x
                | Token.Hole x -> Hole (buildMemberExp bindingContext x)
                | Token.For (ident, acc) ->
                    let accExp = buildMemberExp bindingContext acc
                    let tvarIdent = tvargen.Next()
                    For (
                        TVal.create ident.range tvarIdent bindingContext ident.value,
                        accExp,
                        processBody (Map.add ident.value tvarIdent bindingContext))
                | Token.If acc ->
                    If (
                        buildMemberExp bindingContext acc,
                        processBody bindingContext)
                | Token.End ->
                    match scopeDepth with
                    | 0 ->
                        { ranges = [token.range]
                          message = "Closing a scope is not possible without having a scope open." }
                        |> TrullaException
                        |> raise
                    | n ->
                        scopeDepth <- n-1
                        endTokenDetected <- true
            ]
        pointer,tree,scopeDepth
    try 
        let _,tree,scopeDepth = toTree 0 0 Map.empty
        if scopeDepth > 0 then
            // TODO: Range.zero is wrong
            { ranges = [Range.zero]; message = "TODO: Unclosed scope detected." }
            |> List.singleton
            |> Error
        else
            Ok tree
    with TrullaException err ->
        Error [err]

type Typ =
    | Mono of string
    | Poly of name: string * typParam: Typ
    | Field of Field
    | Record of TVar
    | Var of TVar
and Field = string * Typ

// TODO: After solving, a transition should happen from Type to FinalTyp
////type FinalTyp =
////    | FMono of string
////    | FPoly of name: string * typParam: FinalTyp
////    | FField of Field
////    | FRecord of TVar

// TODO: Problem should beTypr * Type; later, resolve the ranges
type ProblemData = TVar * Typ
type SolutionData = TVar * Typ // should be FinalTyp
type Problem =
    | Unsolved of ProblemData
    | Solved of SolutionData

module KnownTypes =
    // TODO: reserve these keywords + parser tests
    let [<Literal>] string = "string"
    let [<Literal>] bool = "bool"
    let [<Literal>] sequence = "sequence"
    let sequenceOf elemTypId = sequence, elemTypId

// TODO: Prevent shadowing
let buildProblems (tree: TExp list) =
    let mutable possibleRecordNames = []

    let rec constrainMemberExp (membExp: TVal<MemberExp>) =
        match membExp.value with
        | AccessExp accExp ->
            [
                yield! constrainMemberExp accExp.instanceExp
                yield Unsolved (accExp.instanceExp.tvar, Field (accExp.memberName, Var membExp.tvar))
                do possibleRecordNames <-
                    (accExp.instanceExp.tvar, MemberExp.getLastSegment accExp.instanceExp.value)
                    :: possibleRecordNames
            ]
        | IdentExp ident ->
            let tvarIdent = membExp.bindingContext |> Map.tryFind ident
            match tvarIdent with
            | Some tvarIdent -> [ Unsolved (tvarIdent, Var membExp.tvar) ]
            | None -> [ Unsolved (Root, Field (ident, Var membExp.tvar)) ]
    
    let rec constrainTree (tree: TExp list) =
        [ for tree in tree do
            match tree with
            | Text _ -> ()
            | Hole hole ->
                let holeProblems = constrainMemberExp hole
                yield! holeProblems
                yield Unsolved (hole.tvar, Mono KnownTypes.string)
            | For (ident,source,children) ->
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
        ]
    
    let problems = constrainTree tree
    problems,possibleRecordNames

let rec subst tvarToReplace withTyp inTyp =
    ////printfn $"Substing: {tvarToReplace} in {inTyp}"
    let withTyp =
        match withTyp with
        | Field _ -> Record tvarToReplace
        | _ -> withTyp
    match inTyp with
    | Poly (name, inTyp) -> Poly (name, subst tvarToReplace withTyp inTyp)
    | Field (fn,ft) -> Field (fn, subst tvarToReplace withTyp ft)
    | Var tvar when tvar = tvarToReplace -> withTyp
    | Record tvarRec ->
        match withTyp with
        | Var tvar when tvarRec = tvar -> Record tvar
        | _ -> inTyp
    | Mono _
    | Var _ -> inTyp

type Unification =
    | Unified of Problem list
    | KeepOriginal

let solveProblems (problems: Problem list) =
    let rec unify t1 t2 =
        ////printfn $"Unifying: ({t1})  --  ({t2})"
        // TODO: Correct range mapping when constructing new problems
        match t1,t2 with
        | t1,t2 when t1 = t2 -> 
            Unified []
        | Var tv1, Var tv2 ->
            Unified [ Unsolved (tv2, Var tv1) ] // TODO: Why does tv2,tv2 work, but not tv1,tv2?
        | Var tvar, t
        | t, Var tvar ->
            Unified [ Unsolved (tvar, t) ]
        | Poly (n1,pt1), Poly (n2,pt2) when n1 = n2 ->
            unify pt1 pt2
        | Record tvarRec, (Field _ as r)
        | (Field _ as r), Record tvarRec ->
            Unified [ Unsolved (tvarRec, r) ]
        | Field (fn1,ft1), Field (fn2,ft2) ->
            if fn1 = fn2 
                then unify ft1 ft2
                else KeepOriginal
        | _ ->
            // TODO: Don't raise
            { ranges = [] // TODO
              message = $"TODO: Can't unitfy types {t1} and {t2}" }
            |> TrullaException
            |> raise
        
    let substInProblems tvarToReplace withType (inProblems: ProblemData list) newProblem =
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
            problems |> partitionMap (
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

type UnificationResult =
    { tvar: TVar
      errors: string list
      resultingTyp: Typ }

let buildRecords (solution: SolutionData list) =
    solution
    |> List.choose (fun (tvar,t) ->
        match t with
        | Field f -> Some (tvar,f)
        | _ -> None)
    |> List.groupBy fst
    |> List.map (fun (tvar, fields) -> tvar, fields |> List.map snd)
    |> Map.ofList

type SolveResult =
    { 
        tree: TExp list
        records: Map<TVar, Field list>
        possibleRecordNames: (TVar * string) list
    }

let solve tokens =
    result {
        let! tokens = tokens 
        let! tree = buildTree tokens
        let problems,possibleRecordNames = buildProblems tree
        let! solution = solveProblems problems
        let records = buildRecords solution
        return
            {
                tree = tree
                records = records
                possibleRecordNames = possibleRecordNames
            }
    }
    