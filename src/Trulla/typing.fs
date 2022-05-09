module Trulla.Typing

open Parsing

type Typ =
    | Mono of MonoTyp
    | Poly of PolyTyp
and MonoTyp =
    | Bool
    | String
and PolyTyp =
    | List of MonoTyp

type SymbolContext = Map<string, Typ>
    
type Node =
    | Token of Token
    | Scope of ScopedNode
and ScopedNode = { root: PExp; children: Node list } // TODO: root should not be PExp, but For or If
////and ScopedNode = { context: SymbolContext; nodes: Node list }

// TODO: meaningful error messages + location
// TODO: Performance?
let toTree (tokens: Token list) =
    let mutable openScopesCount = -1
    let rec toTree (pointer: int) =
        openScopesCount <- openScopesCount + 1
        let mutable pointer = pointer
        let nodes = [
            let mutable run = true
            while run && pointer < tokens.Length do
                let token = tokens[pointer]
                do pointer <- pointer + 1
                match token with
                | Text _
                | PExp (Hole _) ->
                    yield Token token
                | PExp (For _ as pexp)
                | PExp (If _ as pexp) ->
                    let newPointer,children = toTree pointer
                    yield Scope { root = pexp; children = children }
                    pointer <- newPointer
                | PExp End ->
                    // TODO: Detect error when ending with no begin
                    openScopesCount <- openScopesCount - 1
                    if openScopesCount < 0 then
                        failwith "TODO: Closing an unopened scope"
                    run <- false
            ]
        pointer,nodes
    let res = toTree 0 |> snd
    if openScopesCount > 0
        then failwith "TODO: Unclosed scope detected."
        else res
        
    