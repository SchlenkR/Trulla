module Trulla.Typing

open Parsing

type TemplateError = { message: string; range: Range }

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

type TypeId = TypeId of string list
type TVar = TVar of int
type Type =
    | Var of TVar
    | Mono of TypeId
    | Poly of name:string * typParam:Type
    | Record of RecordDef
    | Any
and RecordDef = { id: TypeId; fields: FieldDef list }
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
    let string = TypeId ["string"]
    let bool = TypeId ["bool"]
    let sequence elemTypId = "sequence", elemTypId

module List =
    let rollOut elements =
        let buildRes parent fieldName isLast = {| parent = parent; fieldName = fieldName; isLast = isLast |}
        let rec rollOut elements fieldName =
            [ match elements with
              | [] -> ()
              | x :: (_::_ as remaining) ->
                  yield buildRes fieldName x false
                  yield! rollOut remaining (fieldName @ [x])
              | [x] ->
                  yield buildRes fieldName x true
            ]
        rollOut elements []

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
            let tvarExp = newTVar()
            let tvarInstance,instanceProblems = constrainExp bindingContext exp.instanceExp
            let problems = [
                Problem (tvarInstance, IsRecordDefinition)
                Problem (tvarInstance, HasField { name = exp.memberName; typ = Var tvarExp})
                yield! instanceProblems
            ]
            tvarExp,problems
        | IdentExp ident ->
            let tvar = bindingContext |> Map.tryFind ident |> Option.defaultWith newTVar
            tvar,[]
    
    let rec buildConstraints (trees: Tree list) (bindingContext: BindingContext) =
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) ->
                ()
            | LeafNode (Hole hole) ->
                let tvarHole,holeProblems = constrainExp bindingContext hole.value
                yield Problem (tvarHole, IsOfType (Mono KnownTypes.string))
                yield! holeProblems
            | InternalNode (For (ident,source), children) ->
                let tvarIdent = newTVar()
                let bindingContext = bindingContext |> Map.add ident.value tvarIdent
                let tvarSource,sourceProblems = constrainExp bindingContext source.value
                yield Problem (tvarSource, IsOfType (Poly (KnownTypes.sequence (Var tvarIdent))))
                yield! sourceProblems
                // --->
                yield! buildConstraints children bindingContext
            | InternalNode (If cond, children) ->
                let tvarCond,condProblems = constrainExp bindingContext cond.value
                yield Problem (tvarCond, IsOfType (Mono KnownTypes.bool))
                yield! condProblems
                // --->
                yield! buildConstraints children bindingContext
        ]
    buildConstraints trees Map.empty

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
