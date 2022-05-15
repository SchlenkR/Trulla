module Trulla.Typing

open Parsing

type TemplateError = { message: string; range: Range } // TODO: Position, not range

// scopes are oriented to rendering; not to typing (e.g. IF would not be internal node in typing)
type Tree =
    | LeafNode of LeafToken
    | InternalNode of root: ScopeToken * children: Tree list
and LeafToken =
    | Text of string
    | Hole of PVal<AccessExp>
and ScopeToken =
    | For of ident: PVal<string> * exp: PVal<AccessExp>
    | If of PVal<AccessExp>

// TODO: meaningful error messages + location
// TODO: Don't throw; return TemplateError results
let buildTree (tokens: ParseResult) : Tree list =
    let mutable scopeStack = []
    let rec toTree (pointer: int) =
        let mutable pointer = pointer
        let nodes = [
            let mutable endTokenDetected = false
            while not endTokenDetected && pointer < tokens.Length do
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

type Type =
    | Prim of PrimTyp
    | Sequence of Type
    | Ref of TypeId
    | Record of {| id: TypeId; fields: RecordField list |}
    | Any
and PrimTyp =
    | Bool
    | Str
and RecordField = { name: string; typ: Type }

type Constraint =
    | IsOfType of Type
    | IsRecord
    | HasField of RecordField

// TODO: DU
type Ident = string

type ExprConstraint = { typeId: TypeId; range: Range; constr: Constraint }

let buildConstraints (trees: Tree list) : ExprConstraint list =
    let newTypeId =
        let mutable x = -1
        fun () ->
            x <- x + 1
            TypeId [ $"'T{x}" ]
    let resolveAccExp boundSymbols acc : TypeId =
        let head =
            match boundSymbols |> Map.tryFind acc.ident with
            | None -> [acc.ident]
            | Some (TypeId tid) -> tid
        TypeId (head @ acc.propPath)
    let constrainAccessExp (boundSymbols: Map<Ident, TypeId>) pvalAccExp finalType =
        let (TypeId tid) = resolveAccExp boundSymbols pvalAccExp.value
        let makeConstraint tid constr = { typeId = TypeId tid; range = pvalAccExp.range; constr = constr }
        let rec constrain (last: string list) curr (remaining: string list) =
            [
                match last,curr,remaining with
                | [], curr, [] ->
                    yield makeConstraint [curr] (IsOfType finalType)
                | last, curr, [] ->
                    yield makeConstraint last IsRecord
                    yield makeConstraint last (HasField { name = curr; typ = finalType })
                | last, curr, x::xs ->
                    yield! constrain (last @ [curr]) x xs
            ]
        match tid with
        | x::xs -> constrain [] x xs
        | [] -> failwith "TODO: fix modelling error (see comment above)" // TODO
    let rec symbolTypes (trees: Tree list) (boundSymbols: Map<Ident, TypeId>) =
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) ->
                ()
            | LeafNode (Hole hole) ->
                yield! constrainAccessExp boundSymbols hole (Prim Str)
            | InternalNode (For (ident,source), children) ->
                let newTypeId = newTypeId()
                yield! constrainAccessExp boundSymbols source (Sequence (Ref newTypeId))
                let newBoundSymbols = boundSymbols |> Map.add ident.value newTypeId
                yield! symbolTypes children newBoundSymbols
            | InternalNode (If cond, children) ->
                yield! constrainAccessExp boundSymbols cond (Prim Bool)
                yield! symbolTypes children boundSymbols
        ]
    symbolTypes trees Map.empty

type UnificationResult =
    { errors: TemplateError list
      resultingTyp: Type }

let buildTypes (constraints: ExprConstraint list) =
    constraints
    |> List.groupBy (fun x -> x.typeId)
    |> List.collect (fun (typeId,constraints) ->
        let result =
            ({ errors = []; resultingTyp = Any }, constraints)
            ||> List.fold (fun state constr ->
                match constr with
                | IsOfType typ ->
                    match state.resultingTyp, typ with
                    | Any,typ
                    | typ,Any -> { state with resultingTyp = typ }
                    | a,b when a = b -> state
                    | a,b ->
                        let err = // TODO: Message
                            { message = $"TODO: Should be '{a}', but is infered to be '{b}'."
                              range = snd constr.typeIdAndRange }
                        { state with errors = () :: state.errors }
                | IsRecord ->
                    match state.resultingTyp with
                    | Any -> { state with resultingTyp = Record {| id = typeId; fields = [] |} }
                    | Record -> state
            )
        result
    )
