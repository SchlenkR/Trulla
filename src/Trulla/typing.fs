module Trulla.Typing

open Parsing

type Tree =
    | LeafNode of PositionalValue<LeafToken>
    | InternalNode of PositionalValue<ScopeToken> * Tree list
and LeafToken =
    | Text of string
    | Hole of AccessExp
and ScopeToken =
    | For of ident: string * exp: AccessExp
    | If of AccessExp

type Type =
    | Mono of MonoTyp
    | Poly of PolyTyp
and MonoTyp =
    | Bool
    | String
    | Record of {| propName: string; type': Type |} list
and PolyTyp =
    | List of MonoTyp

// TODO: meaningful error messages + location
// TODO: Performance?
let toTree (tokens: ParseResult) : Tree list =
    let res,openScopesCount =
        let mutable openScopesCount = -1
        let rec toTree (pointer: int) =
            openScopesCount <- openScopesCount + 1
            let mutable pointer = pointer
            let nodes = 
                [
                    let mutable run = true
                    while run && pointer < tokens.Length do
                        let token = tokens[pointer]
                        pointer <- pointer + 1
                        let newToken value =
                            { value = value; start = token.start; finish = token.finish }
                        let descent value =
                            let newPointer,children = toTree pointer
                            let res = InternalNode (newToken value, children)
                            pointer <- newPointer
                            res
                        match token.value with
                        | ParserToken.Text x -> yield LeafNode (newToken (Text x))
                        | ParserToken.Hole x -> yield LeafNode (newToken (Hole x))
                        | ParserToken.For (ident, acc) -> yield descent (For (ident, acc))
                        | ParserToken.If acc -> yield descent (If acc)
                        | ParserToken.End ->
                            run <- false
                            openScopesCount <- openScopesCount - 1
                            if openScopesCount < 0 then
                                failwith "TODO: Closing an unopened scope"
                ]
            pointer,nodes
        snd(toTree 0), openScopesCount
    if openScopesCount > 0
        then failwith "TODO: Unclosed scope detected."
    res

let typeTree tree =
    let rec typeTree (trees: Tree list) typeConstraints (boundSymbols: List<string * AccessExp>) =
        for tree in trees do
            match tree with
            | LeafNode {value = Text _} ->
                typeConstraints
            | LeafNode ({value = Hole exp} as posToken) ->
                typeConstraints
            | InternalNode (positionalToken, trees) ->
                match positionalToken.value with
                | For (ident, exp) ->
                    let typeConstraints = typeConstraints
                    let boundSymbols = (ident, exp) :: boundSymbols
                    typeTree trees typeConstraints boundSymbols
                | If ident ->
                    let typeConstraints = typeConstraints
                    typeTree trees typeConstraints boundSymbols
    ()
