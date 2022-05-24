module Trulla.Internal.Typing

open Parsing

type Tree =
    | LeafNode of LeafToken
    | InternalNode of root: ScopeToken * children: Tree list
and LeafToken =
    | Text of string
    | Hole of PVal<Exp>
and ScopeToken =
    | For of ident: PVal<string> * exp: PVal<Exp>
    | If of PVal<Exp>

// TODO: meaningful error messages + location
// TODO: Don't throw; return TemplateError results
let buildTree (tokens: PVal<ParserToken> list) =
    let mutable scopeStack = []
    let rec toTree (pointer: int) =
        let mutable pointer = pointer
        let nodes = 
            let mutable endTokenDetected = false
            [ while not endTokenDetected && pointer < tokens.Length do
                let token = tokens[pointer]
                pointer <- pointer + 1
                let descentWithNewScope scopeToken =
                    scopeStack <- scopeToken :: scopeStack
                    let newPointer,children = toTree pointer
                    let res = InternalNode (scopeToken, children)
                    pointer <- newPointer
                    res
                match token.value with
                | ParserToken.Text x -> yield LeafNode (Text x)
                | ParserToken.Hole x -> yield LeafNode (Hole x)
                | ParserToken.For (ident, acc) -> yield descentWithNewScope (For (ident, acc))
                | ParserToken.If acc -> yield descentWithNewScope (If acc)
                | ParserToken.End ->
                    match scopeStack with
                    | [] ->
                        { ranges = [token.range]
                          message = "Closing a scope is not possible without having a scope open." }
                        |> TrullaException
                        |> raise
                    | _ :: xs -> scopeStack <- xs
            ]
        pointer,nodes
    try 
        let tree = snd (toTree 0)
        if scopeStack.Length > 0 then
            // TODO: Position.zero is wrong
            { ranges = [Position.zero |> Position.toRange]
              message ="TODO: Unclosed scope detected." }
            |> Error
        else
            Ok tree
    with TrullaException err ->
        Error err

type TVar =
    | TVar of int
    | Root

type Type =
    | Mono of string
    | Poly of name: string * typParam: Type
    | RecDef of Field list
    | RecRef of TVar
    | Var of TVar // TODO: why VAR is in here?
    override this.ToString() =
        match this with
        | Mono s -> s
        | Poly (n,tp) -> $"%O{n}<%O{tp}>"
        | RecDef fields ->
            let renderField (fn,ft) = $"""{fn}: %O{ft}"""
            let fields = fields |> List.map renderField |> String.concat "; "
            $"""{{ {fields} }}"""
        | Var tvar -> string tvar
        | RecRef tvar -> $"""(RecRef {match tvar with Root -> "ROOT" | TVar tvar -> $"{tvar}"})"""
and Field = string * Type

type BindingContext = Map<string, TVar>

type Problem = Problem of TVar * PVal<Type>

module KnownTypes =
    // TODO: reserve these keywords + parser tests
    let string = "string"
    let bool = "bool"
    let sequence elemTypId = "sequence", elemTypId

type TVarGen() =
    let mutable x = -1
    let mutable rangeToTVar = Map.empty
    member this.RangeToTVar = rangeToTVar
    member this.Next forRange =
        x <- x + 1
        rangeToTVar <- rangeToTVar |> Map.add forRange (TVar x)
        TVar x

// TODO: Prevent shadowing
let collectConstraints (trees: Tree list) =
    let tvargen = TVarGen()
    
    let rec constrainExp (bindingContext: BindingContext) (exp: PVal<Exp>) =
        match exp.value with
        | AccessExp accExp ->
            let tvarInstance,instanceProblems = constrainExp bindingContext accExp.instanceExp
            let tvarExp = tvargen.Next(exp.range)
            let problems = [
                yield! instanceProblems
                yield Problem (tvarInstance, PVal.create exp.range (RecDef [accExp.memberName, Var tvarExp]))
            ]
            tvarExp,problems
        | IdentExp ident ->
            let tvarIdent = bindingContext |> Map.tryFind ident.value
            match tvarIdent with
            | Some tvarIdent ->
                tvarIdent,[]
            | None ->
                let tvarIdent = tvargen.Next(ident.range)
                let problems = [
                    yield Problem (Root, PVal.create exp.range (RecDef [ident.value, Var tvarIdent]))
                ]
                tvarIdent,problems
    
    let rec buildConstraints (trees: Tree list) (bindingContext: BindingContext) =
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) ->
                ()
            | LeafNode (Hole hole) ->
                let tvarHole,holeProblems = constrainExp bindingContext hole
                yield! holeProblems
                yield Problem (tvarHole, PVal.create hole.range (Mono KnownTypes.string))
            | InternalNode (For (ident,source), children) ->
                let tvarIdent = tvargen.Next(ident.range)
                let bindingContext = bindingContext |> Map.add ident.value tvarIdent
                let tvarSource,sourceProblems = constrainExp bindingContext source
                yield! sourceProblems
                yield Problem (tvarSource, PVal.create source.range (Poly (KnownTypes.sequence (Var tvarIdent))))
                // --->
                yield! buildConstraints children bindingContext
            | InternalNode (If cond, children) ->
                let tvarCond,condProblems = constrainExp bindingContext cond
                yield! condProblems
                yield Problem (tvarCond, PVal.create cond.range (Mono KnownTypes.bool))
                // --->
                yield! buildConstraints children bindingContext
        ]
    buildConstraints trees Map.empty

let rec subst tvarToReplace withTyp inTyp =
    let withTyp = match withTyp with RecDef _ -> RecRef tvarToReplace | _ -> withTyp
    match inTyp with
    | Poly (name, inTyp) -> Poly (name, subst tvarToReplace withTyp inTyp)
    | RecDef fields -> RecDef [ for fn,ft in fields do fn, subst tvarToReplace withTyp ft ]
    | Var tvar when tvar = tvarToReplace -> withTyp
    | _ -> inTyp

let rec unify originalTvar t1 t2 =
    printfn $"Unifying: ({t1.value})  --  ({t2.value})"
    [
        // TODO: Correct range mapping when constructing new problems
        match t1.value, t2.value with
        | t1,t2 when t1 = t2 -> ()
        | Var _, Var tvar ->
            printfn "YIELD a"
            yield Problem (tvar, t1)
        | Var tvar, t
        | t, Var tvar ->
            printfn "YIELD b"
            yield Problem (tvar, {t1 with value = t})
        | Poly (n1,pt1), Poly (n2,pt2) when n1 = n2 ->
            printfn "YIELD c ->"
            yield! unify originalTvar {t1 with value = pt1} {t2 with value = pt2}
        | RecRef recref, (RecDef _ as r)
        | (RecDef _ as r), RecRef recref ->
            printfn "YIELD d"
            yield Problem (recref, {t1 with value = r})
        | RecDef f1, RecDef f2 ->
            let unifiedFields =
                (f1 @ f2)
                |> List.groupBy fst
                |> List.map (fun (fname, ftypes) ->
                    let ftype,problems =
                        ftypes
                        |> List.map (fun (_,ft) -> ft,[])
                        |> List.reduce (fun (ft1,problems) (ft2,_) ->
                            ft1, problems @ unify originalTvar {t1 with value = ft1} {t2 with value = ft2}
                        )
                    (fname,ftype),problems
                )
            let fields = unifiedFields |> List.map fst
            let problems = unifiedFields |> List.collect snd
            let record = Problem (originalTvar, PVal.create t1.range (RecDef fields))
            yield! record :: problems
        | _ ->
            { ranges = [t1.range; t2.range]
              message = $"TODO: Can't unitfy types {t1.value} and {t2.value}" }
            |> TrullaException
            |> raise
    ]
    
let substMany tvarToReplace withType inProblems =
    [ for (Problem (ptvar, ptype)) in inProblems do
        let ptype = { ptype with value = subst tvarToReplace withType.value ptype.value }
        match ptvar = tvarToReplace with
        | false -> yield Problem (ptvar, ptype)
        | true -> yield! unify ptvar withType ptype
    ]

let solveProblems (problems: Problem list) =
    let rec solve problems solution =
        match problems with
        | [] -> solution
        | (Problem (tvar, typ) as p) :: ps ->
            let problems = substMany tvar typ ps
            let solution = p :: substMany tvar typ solution
            solve problems solution
    // TODO: Comment why distinct is needed
    try solve problems [] |> List.distinct |> Ok
    with TrullaException err -> Error err

type UnificationResult =
    { tvar: TVar
      errors: string list
      resultingTyp: Type }

let buildRecords (problems: Problem list) =
    problems
    |> List.map (fun (Problem (tvar,t)) -> tvar,t)
    |> List.choose (fun (tvar,t) -> match t.value with RecDef f -> Some (tvar,f) | _ -> None)
    |> List.groupBy fst
    |> List.map (fun (tvar, fields) -> tvar, fields |> List.map snd |> List.distinct)
    |> Map.ofList
