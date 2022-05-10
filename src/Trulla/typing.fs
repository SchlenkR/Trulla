module Trulla.Typing

open Parsing

type Tree =
    | LeafNode of PositionalValue<LeafToken>
    | InternalNode of PositionalValue<ScopeToken> * Tree list

type Typ =
    | Mono of MonoTyp
    | Poly of PolyTyp
and MonoTyp =
    | Bool
    | String
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
                        match token.value with
                        | LeafToken x ->
                            yield LeafNode { value = x; start = token.start; finish = token.finish }
                        | ScopeToken x ->
                            let newPointer,children = toTree pointer
                            yield InternalNode ({ value = x; start = token.start; finish = token.finish }, children)
                            pointer <- newPointer
                        | StupidToken End ->
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

//let typeTree (Root tree) =
//    let rec typeTree (nodes: Node list) =
//        for node in nodes do
//            match node with
//            | Token token -> token.value
//    ()

