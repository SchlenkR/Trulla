module Trulla.Typing

open Parsing

type Tree =
    | LeafNode of Token<LeafToken>
    | InternalNode of Token<InternalToken> * Tree list
and LeafToken =
    | Text of string
    | Hole of Access
and InternalToken =
    | For of ident: string * source: Access
    | If of Access

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
let toTree (tokens: TmplToken list) : Tree list =
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
                        let tok x = { value = x; start = token.start; finish = token.finish }
                        pointer <- pointer + 1
                        let descent x =
                            let newPointer,children = toTree pointer
                            let res = InternalNode (x, children)
                            pointer <- newPointer
                            res
                        match token.value with
                        | Parsing.Text text ->
                            yield LeafNode (Text text |> tok)
                        | Parsing.Hole exp ->
                            yield LeafNode (Hole exp|> tok)
                        | Parsing.For (ident, source) ->
                            yield descent (For (ident, source) |> tok)
                        | Parsing.If exp ->
                            yield descent (If exp |> tok)
                        | Parsing.End ->
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

