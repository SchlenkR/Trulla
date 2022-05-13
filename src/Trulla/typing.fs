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
let tree (tokens: ParseResult) : Tree list =
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

type Type =
    | Mono of MonoTyp
    | Poly of PolyTyp
    | Any
    | Var of Tyvar
and MonoTyp =
    | Bool
    | Str
    | Record of RecordDef
and PolyTyp =
    | Sequence of Type
and RecordDef = { id: TypeId; fields: FieldDef list }
and FieldDef = { name: string; type': Type }
and TypeId = TypeId of string list
// TODO: DU
and Ident = string
and Tyvar = string

// TODO: split into (range - typeId), (typeId - constr) and get rid of option(range)?
type Constraint = { range: Range option; typeId: TypeId; constr: Type }

let constraints (trees: Tree list) : Constraint list =
    let newTypeId =
        let mutable x = -1
        fun () ->
            x <- x + 1
            $"'T{x}"
    let resolve boundSymbols acc : TypeId =
        let head =
            match boundSymbols |> Map.tryFind acc.ident with
            | None -> acc.ident
            | Some tyvar -> tyvar
        TypeId (head :: acc.propPath)
    let constraintsFromTypeId (TypeId tid) =
        let rec constrain (tid: string list) =
            match tid with
            | [] -> failwith "TODO: TypeId must not be a list, but root * list" // TODO
            | [rootTypeId] as tid -> { range = None; typeId = TypeId tid; constr = Any }
            | x :: xs -> failwith ""
        constrain tid
    let rec symbolTypes (trees: Tree list) (boundSymbols: Map<Ident, Tyvar>) =
        let constrainAccessExp pvalAccExp typ =
            let typeId = resolve boundSymbols pvalAccExp.value
            let typeConstraints = constraintsFromTypeId typeId
            let constr = { range = Some pvalAccExp.range; typeId = typeId; constr = typ }
            constr :: typeConstraints
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) -> ()
            | LeafNode (Hole hole) ->
                yield! constrainAccessExp hole (Mono Str)
            | InternalNode (For (ident,source), children) ->
                let newTypeId = newTypeId()
                let boundSymbols = boundSymbols |> Map.add ident.value newTypeId
                let sourceTypeId = resolve boundSymbols source.value
                let sourceExprConstraints = constraintsFromTypeId sourceTypeId
                yield! sourceExprConstraints
                yield { range = Some source.range
                        typeId = sourceTypeId
                        constr = Poly (Sequence (Var newTypeId))
                        }
                yield! symbolTypes children boundSymbols
            | InternalNode (If cond, children) ->
                yield! constrainAccessExp cond (Mono Bool)
                yield! symbolTypes children boundSymbols
        ]
    symbolTypes trees Map.empty
