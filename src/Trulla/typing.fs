module Trulla.Internal.Typing

open Helper
open Parsing

type TVar = TVar of int | Root

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
    member this.Next() = x <- x + 1; TVar x

module TVal =
    let create range tvar bindingContext value =
        { range = range; tvar = tvar; bindingContext = bindingContext; value = value }

// TODO: meaningful error messages + location
// TODO: Don't throw; return TemplateError results
let buildTree (tokens: PVal<Token> list) : Result<TExp list, TrullaError list> =
    let tvargen = TVarGen()

    let buildMemberExp bindingContext pexp : TVal<MemberExp> =
        let rec ofPExpZero (pexp: PVal<Parsing.MemberToken>) =
            let newTVal value = TVal.create pexp.range (tvargen.Next()) bindingContext value
            match pexp.value with
            | AccessToken accExp ->
                let accExp = {| instanceExp = ofPExpZero accExp.instanceExp; memberName = accExp.memberName |}
                newTVal  (AccessExp accExp)
            | IdentToken identExp ->
                newTVal (IdentExp identExp)
        ofPExpZero pexp

    let rec toTree (pointer: int) scopeDepth (bindingContext: BindingContext) =
        // TODO: IS all that really necessary - the mutable stuff?
        let mutable pointer = pointer
        let mutable scopeDepth = scopeDepth

        let tree = 
            let mutable endTokenDetected = false
            [ while not endTokenDetected && pointer < tokens.Length do
                let token = tokens[pointer]
                pointer <- pointer + 1

                let processBody bindingContext : TExp list =
                    scopeDepth <- scopeDepth + 1
                    let newPointer,children,newScopeDepth = toTree pointer scopeDepth bindingContext
                    scopeDepth <- newScopeDepth
                    pointer <- newPointer
                    children

                match token.value with
                | Token.Text x -> Text x
                | Token.Hole x -> Hole (buildMemberExp bindingContext x)
                | Token.For (ident, acc) ->
                    let tvarIdent = tvargen.Next()
                    For (
                        TVal.create ident.range tvarIdent bindingContext ident.value,
                        buildMemberExp bindingContext acc,
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
                    | n -> scopeDepth <- n-1
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

type Type =
    | Mono of string
    | Poly of name: string * typParam: Type
    | RecDef of Field
    | RecRef of TVar
    | Var of TVar // TODO: why VAR is in here?
    override this.ToString() =
        match this with
        | Mono s -> s
        | Poly (n,tp) -> $"%O{n}<%O{tp}>"
        | RecDef (fn,ft) -> $"""{fn}: %O{ft}"""
        | Var tvar -> string tvar
        | RecRef tvar -> $"""(RecRef {match tvar with Root -> "ROOT" | TVar tvar -> $"{tvar}"})"""
and Field = string * Type

// TODO: Problem should be TVar * Type; later, resolve the ranges
type ProblemData = TVar * Type
type Problem =
    | Unsolved of ProblemData
    | Solved of ProblemData

module KnownTypes =
    // TODO: reserve these keywords + parser tests
    let [<Literal>] string = "string"
    let [<Literal>] bool = "bool"
    let [<Literal>] sequence = "sequence"
    let sequenceOf elemTypId = sequence, elemTypId

// TODO: Prevent shadowing
let buildProblems (tree: TExp list) =
    let rec constrainMemberExp (membExp: TVal<MemberExp>) =
        match membExp.value with
        | AccessExp accExp ->
            [
                yield! constrainMemberExp accExp.instanceExp
                yield Unsolved (accExp.instanceExp.tvar, RecDef (accExp.memberName, Var membExp.tvar))
            ]
        | IdentExp ident ->
            let tvarIdent = membExp.bindingContext |> Map.tryFind ident
            match tvarIdent with
            | Some _ -> []
            | None -> [ Unsolved (Root, RecDef (ident, Var membExp.tvar)) ]
    
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
    constrainTree tree

let rec subst tvarToReplace withTyp inTyp =
    let withTyp = match withTyp with RecDef _ -> RecRef tvarToReplace | _ -> withTyp
    match inTyp with
    | Poly (name, inTyp) -> Poly (name, subst tvarToReplace withTyp inTyp)
    | RecDef (fn,ft) -> RecDef (fn, subst tvarToReplace withTyp ft)
    | Var tvar when tvar = tvarToReplace -> withTyp
    | _ -> inTyp

let rec unify originalTvar t1 t2 =
    [
        printfn $"Unifying: ({t1})  --  ({t2})"

        // TODO: Correct range mapping when constructing new problems
        match t1, t2 with
        | t1,t2 when t1 = t2 -> ()
        | Var _, Var tvar ->
            printfn "YIELD a"
            yield Unsolved (tvar, t1)
        | Var tvar, t
        | t, Var tvar ->
            printfn "YIELD b"
            yield Unsolved (tvar, t)
        | Poly (n1,pt1), Poly (n2,pt2) when n1 = n2 ->
            printfn "YIELD c ->"
            yield! unify originalTvar pt1 pt2
        | RecRef tvarRec, (RecDef _ as r)
        | (RecDef _ as r), RecRef tvarRec ->
            printfn "YIELD d"
            yield Unsolved (tvarRec, r)
        | RecDef (fn1,ft1), RecDef (fn2,ft2) ->
            if fn1 = fn2 then
                yield! unify originalTvar ft1 ft2
            //let unifiedFieldsAndProblems =
            //    // TODO: Use partitionMap
            //    (f1 @ f2)
            //    |> List.groupBy fst
            //    |> List.map (fun (fname, ftypes) ->
            //        // We finally use only ft1. This works because 'unify' only needs to emit
            //        // new problems, but it doesn't resolve 2 types to one by a type hierarchy or
            //        // something like that; out type system only has "equal" or "unequal" types.
            //        let ftype,problems =
            //            ftypes
            //            |> List.map (fun (_,ft) -> ft,[])
            //            |> List.reduce (fun (ft1,problems) (ft2,_) ->
            //                ft1, problems @ unify originalTvar {t1 with value = ft1} {t2 with value = ft2}
            //            )
            //        (fname,ftype),problems
            //    )
            //let fields = unifiedFieldsAndProblems |> List.map fst
            //let res = [
            //    yield Problem (originalTvar, TVal.create t1.range t1.tvar t1.bindingContext (RecDef fields))
            //    yield! unifiedFieldsAndProblems |> List.collect snd
            //    ]
            //printfn $"Resulting RecordProblems: %A{res}"
            //yield! res
        | _ ->
            { ranges = [] // TODO
              message = $"TODO: Can't unitfy types {t1} and {t2}" }
            |> TrullaException
            |> raise
    ]
    
let substInProblems tvarToReplace withType (inProblems: ProblemData list) =
    [ for ptvar, ptype in inProblems do
        let ptype = subst tvarToReplace withType ptype
        match ptvar = tvarToReplace with
        | false -> yield Solved (ptvar, ptype)
        | true -> yield! unify ptvar withType ptype
    ]

let solveProblems (problems: Problem list) =
    let rec solve (problems: Problem list) =
        let solved,unsolved = problems |> partitionMap (function Solved x -> Choice1Of2 x | Unsolved x -> Choice2Of2 x)
        printfn $"Solved count: {solved.Length}   -   unsolved count: {unsolved.Length}"
        match solved,unsolved with
        | solved,[] -> solved
        | solved, ((tvar, typ) as p) :: ps ->
            solve [
                yield! substInProblems tvar typ ps
                yield Solved p
                yield! substInProblems tvar typ solved
            ]
    try Ok (solve problems)
    with TrullaException err -> Error [err]

type UnificationResult =
    { tvar: TVar
      errors: string list
      resultingTyp: Type }

let buildRecords (solution: ProblemData list) =
    failwith "Fotze"
    //solution
    //|> List.map (fun (tvar,t) -> tvar,t)
    //|> List.choose (fun (tvar,t) -> match t.value with RecDef f -> Some (tvar,f) | _ -> None)
    //|> List.groupBy fst
    //|> List.map (fun (tvar, fields) ->
    //    // comment why 'distinct' is needed (merging fields: original problem always remains in solution ist)
    //    tvar, fields |> List.collect snd |> List.distinct)
    //|> Map.ofList

let solve tokens =
    result {
        let! tokens = tokens 
        let! tree = buildTree tokens
        let problems = buildProblems tree
        let! solution = solveProblems problems
        let records = buildRecords solution
        return tree,records
    }
    