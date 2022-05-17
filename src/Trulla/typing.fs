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

type TVar = TVar of int
type Type =
    | Var of TVar
    | Mono of string
    | Poly of name:string * typParam:Type
    | Record of RecordDef
    | Any
and RecordDef = { name: string; fields: FieldDef list }
and FieldDef = { name: string; typ: Type }

type Constraint =
    | IsOfType of Type
    | IsRecordDefinition
    | HasField of FieldDef

type BindingContext = Map<string, TVar>

type Problem =
    | Problem of TVar * Constraint

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
    let tvarRoot = TVar 666 // TODO: REMOVE THIS MAGIC NUMBER!
    
    let rec constrainExp (bindingContext: BindingContext) exp =
        match exp with
        | AccessExp exp ->
            let tvarInstance,instanceProblems = constrainExp bindingContext exp.instanceExp
            let tvarExp = newTVar()
            let problems = [
                yield! instanceProblems
                Problem (tvarInstance, IsRecordDefinition)
                Problem (tvarInstance, HasField { name = exp.memberName; typ = Var tvarExp})
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
                    Problem (tvarRoot, HasField { name = ident; typ = Var tvarIdent })
                    // tvarRoot is also a record; but we can omit this
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
                yield Problem (tvarHole, IsOfType (Mono KnownTypes.string))
            | InternalNode (For (ident,source), children) ->
                let tvarIdent = newTVar()
                let bindingContext = bindingContext |> Map.add ident.value tvarIdent
                let tvarSource,sourceProblems = constrainExp bindingContext source.value
                yield! sourceProblems
                yield Problem (tvarSource, IsOfType (Poly (KnownTypes.sequence (Var tvarIdent))))
                // --->
                yield! buildConstraints children bindingContext
            | InternalNode (If cond, children) ->
                let tvarCond,condProblems = constrainExp bindingContext cond.value
                yield! condProblems
                yield Problem (tvarCond, IsOfType (Mono KnownTypes.bool))
                // --->
                yield! buildConstraints children bindingContext
        ]
    
    // TODO: return also tvarRoot
    buildConstraints trees Map.empty

// TODO: Ranges wieder überall reinmachen
////type TemplateError = { message: string; range: Range }
type UnificationResult =
    { tvar: TVar
      errors: string list
      resultingTyp: Type }

let unifyConstraints (problems: Problem list) =
    problems
    |> List.map (fun (Problem (TVar x,y)) -> x,y)
    |> List.groupBy fst
    |> List.map (fun (tvar, values) ->
        (
            { tvar = TVar tvar; errors = []; resultingTyp = Any },
            values |> List.map snd
        )
        ||> List.fold (fun state constr ->
            let makeRecord fields = Record { name = $"Type{tvar}"; fields = fields }
            
            match constr with
            | IsOfType typ ->
                match typ,state.resultingTyp with
                | Any,typ
                | typ,Any -> { state with resultingTyp = typ }
                | a,b when a = b -> state
                | a,b ->
                    let err = $"TODO: Should be '{a}', but is infered to be '{b}'."
                    { state with errors = err :: state.errors }
            | IsRecordDefinition ->
                // TODO: Better type names
                match state.resultingTyp with
                | Any -> { state with resultingTyp = makeRecord [] }
                | Record _ -> state
                | _ -> { state with errors = "TODO: Record expected" :: state.errors }
            | HasField field ->
                match state.resultingTyp with
                | Any -> { state with resultingTyp = makeRecord [field] }
                | Record r -> { state with resultingTyp = Record { r with fields = field :: r.fields }}
                | _ -> { state with errors = "TODO: Record expected" :: state.errors }
        )
    )

////type UnificationResult =
////    { typeId: TypeId
////      errors: TemplateError list
////      resultingTyp: Type }

////let unifyConstraints (constraints: ExprConstraint list) =
////    constraints
////    |> List.groupBy (fun x -> x.typeId)
////    |> List.map (fun (typeId,constraints) ->
////        ({ typeId = typeId; errors = []; resultingTyp = Any }, constraints)
////        ||> List.fold (fun state expConstr ->
////            let addRecordExpectedError () =
////                // TODO: Message
////                let err =
////                    { message = $"TODO: Record expected; mono or sequence infered."
////                      range = expConstr.range }
////                { state with errors = err :: state.errors }
////            match expConstr.constr with
////            | IsOfType typ ->
////                match typ,state.resultingTyp with
////                | Any,typ
////                | typ,Any -> { state with resultingTyp = typ }
////                | a,b when a = b -> state
////                | a,b ->
////                    let err = // TODO: Message
////                        { message = $"TODO: Should be '{a}', but is infered to be '{b}'."
////                          range = expConstr.range }
////                    { state with errors = err :: state.errors }
////            | IsRecordDefinition ->
////                match state.resultingTyp with
////                | Any -> { state with resultingTyp = Record { id = typeId; fields = [] }}
////                | Record _ -> state
////                | _ -> addRecordExpectedError ()
////            | HasField field ->
////                match state.resultingTyp with
////                | Any -> { state with resultingTyp = Record { id = typeId; fields = [field] }}
////                | Record r -> { state with resultingTyp = Record { r with fields = field :: r.fields }}
////                | _ -> addRecordExpectedError ()
////        )
////    )
