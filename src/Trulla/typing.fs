module Trulla.Typing

open Parsing

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
let buildTree (tokens: ParseResult) : Tree list =
    let res,openScopesCount =
        let mutable openScopesCount = -1
        let rec toTree (pointer: int) =
            let mutable pointer = pointer
            openScopesCount <- openScopesCount + 1
            let nodes = [
                let mutable isConsistent = true
                while isConsistent && pointer < tokens.Length do
                    let token = tokens[pointer]
                    pointer <- pointer + 1
                    let descent scopeToken =
                        let newPointer,children = toTree pointer
                        let res = InternalNode (scopeToken, children)
                        pointer <- newPointer
                        res
                    match token with
                    | ParserToken.Text x -> yield LeafNode (Text x)
                    | ParserToken.Hole x -> yield LeafNode (Hole x)
                    | ParserToken.For (ident, acc) -> yield descent (For (ident, acc))
                    | ParserToken.If acc -> yield descent (If acc)
                    | ParserToken.End ->
                        isConsistent <- false
                        openScopesCount <- openScopesCount - 1
                        if openScopesCount < 0 then
                            failwith "TODO: Closing an unopened scope"
                ]
            pointer,nodes
        snd(toTree 0), openScopesCount
    if openScopesCount > 0
        then failwith "TODO: Unclosed scope detected."
    res

type TypeId = TypeId of string list

type Type =
    | Prim of PrimTyp
    | Sequence of Type
    | Ref of TypeId
    | Record of {| id: TypeId; fields: RecordField list |}
and PrimTyp =
    | Bool
    | Str
and RecordField = { name: string; typ: Type }

type Constraint =
    | IsType of Type
    | IsRecord
    | HasField of RecordField

// TODO: DU
type Ident = string

type ExprConstraint = { typeIdAndRange: TypeId * Range; constr: Constraint }

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
        let makeConstraint tid constr = { typeIdAndRange = TypeId tid,pvalAccExp.range; constr = constr }
        let rec constrain (last: string list) curr (remaining: string list) =
            [
                match last,curr,remaining with
                | [], curr, [] ->
                    yield makeConstraint [curr] (IsType finalType)
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

let solve (constraints: ExprConstraint list) =

    let groupedConstraints = constraints |> List.groupBy (fun x -> x.typeIdAndRange)

