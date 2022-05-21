module Trulla.Typing

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
let buildTree (tokens: ParseResult) : Tree list =
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
                match token with
                | ParserToken.Text x -> yield LeafNode (Text x)
                | ParserToken.Hole x -> yield LeafNode (Hole x)
                | ParserToken.For (ident, acc) -> yield descentWithNewScope (For (ident, acc))
                | ParserToken.If acc -> yield descentWithNewScope (If acc)
                | ParserToken.End ->
                    match scopeStack with
                    | [] -> failwith "TODO: Closing an unopened scope"
                    | _ :: xs -> scopeStack <- xs
            ]
        pointer,nodes
    let tree = snd (toTree 0)
    if scopeStack.Length > 0
        then failwith "TODO: Unclosed scope detected."
        else tree

type TVar =
    | TVar of int
    | Root

type Type =
    | Mono of string
    | Poly of name: string * typParam: Type
    | Record of Field list
    | RecRef of TVar
    | Var of TVar // TODO: why VAR is in here?
    override this.ToString() =
        match this with
        | Mono s -> s
        | Poly (n,tp) -> $"%O{n}<%O{tp}>"
        | Record fields -> $"""{{ {[for (n,t) in fields do $"{n}: %O{t}"] |> String.concat "; " } }}"""
        | Var tvar -> string tvar
        | RecRef tvar -> 
            let x = match tvar with Root -> "ROOT" | TVar tvar -> $"{tvar}"
            $"(RecRef {x})"
and Field = string * Type

type BindingContext = Map<string, TVar>

type Problem = Problem of TVar * Type

module KnownTypes =
    // TODO: reserve these keywords + parser tests
    let string = "string"
    let bool = "bool"
    let sequence elemTypId = "sequence", elemTypId

// TODO: Prevent shadowing
let collectConstraints (trees: Tree list) =
    let newTVar =
        let mutable x = -1
        fun () ->
            x <- x + 1
            TVar x
    
    let rec constrainExp (bindingContext: BindingContext) exp =
        match exp with
        | AccessExp exp ->
            let tvarInstance,instanceProblems = constrainExp bindingContext exp.instanceExp
            let tvarExp = newTVar()
            let problems = [
                yield! instanceProblems
                yield Problem (tvarInstance, Record [exp.memberName, Var tvarExp])
            ]
            tvarExp,problems
        | IdentExp ident ->
            let tvarIdent = bindingContext |> Map.tryFind ident
            match tvarIdent with
            | Some tvarIdent ->
                tvarIdent,[]
            | None ->
                let tvarIdent = newTVar()
                let problems = [
                    yield Problem (Root, Record [ident, Var tvarIdent])
                ]
                tvarIdent,problems
    
    let rec buildConstraints (trees: Tree list) (bindingContext: BindingContext) =
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) ->
                ()
            | LeafNode (Hole hole) ->
                let tvarHole,holeProblems = constrainExp bindingContext hole.value
                yield! holeProblems
                yield Problem (tvarHole, Mono KnownTypes.string)
            | InternalNode (For (ident,source), children) ->
                let tvarIdent = newTVar()
                let bindingContext = bindingContext |> Map.add ident.value tvarIdent
                let tvarSource,sourceProblems = constrainExp bindingContext source.value
                yield! sourceProblems
                yield Problem (tvarSource, Poly (KnownTypes.sequence (Var tvarIdent)))
                // --->
                yield! buildConstraints children bindingContext
            | InternalNode (If cond, children) ->
                let tvarCond,condProblems = constrainExp bindingContext cond.value
                yield! condProblems
                yield Problem (tvarCond, Mono KnownTypes.bool)
                // --->
                yield! buildConstraints children bindingContext
        ]
    buildConstraints trees Map.empty

type Record = { name: string; fields: Field list }

// TODO: Ranges wieder überall reinmachen
////type TemplateError = { message: string; range: Range }
type UnificationResult =
    { tvar: TVar
      errors: string list
      resultingTyp: Record }

let rec subst tvarToReplace withTyp inTyp =
    let withTyp = match withTyp with Record _ -> RecRef tvarToReplace | _ -> withTyp
    match inTyp with
    | Poly (name, inTyp) ->
        Poly (name, subst tvarToReplace withTyp inTyp)
    | Record fields ->
        Record [ for (n,t) in fields do n, subst tvarToReplace withTyp t ]
    | Var tvar when tvar = tvarToReplace ->
        withTyp
    | _ ->
        inTyp
    
let rec unify originalTvar t1 t2 =
    printfn $"Unifying: ({t1})  --  ({t2})"
    [
        match t1,t2 with
        | t1,t2 when t1 = t2 -> ()
        | Var _, Var tvar ->
            yield Problem (tvar, t1)
        | Var tvar, t
        | t, Var tvar ->
            yield Problem (tvar, t)
        | Poly (n1,t1), Poly (n2,t2) when n1 = n2 ->
            yield! unify originalTvar t1 t2
        | RecRef recref, (Record _ as r)
        | (Record _ as r), RecRef recref ->
            yield Problem (recref, r)
        | Record f1, Record f2 ->
            let res =
                (f1 @ f2)
                |> List.groupBy fst
                |> List.map (fun (fname, types) ->
                    let ftype,problems =
                        types
                        |> List.map (fun (_,ft) -> ft,[])
                        |> List.reduce (fun (t1,problems) (t2,_) ->
                            t1, problems @ unify originalTvar t1 t2
                        )
                    (fname,ftype),problems
                )
            let record = Problem (originalTvar, Record (res |> List.map fst))
            let newProblems = res |> List.collect snd
            yield! record :: newProblems
        | _ ->
            failwith $"TODO: Can't unitfy {t1} and {t2}"
    ]
    
let substMany tvarToReplace withType inProblems =
    [ for (Problem (ptvar, ptype)) in inProblems do
        let ptype = subst tvarToReplace withType ptype
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
    solve problems []
