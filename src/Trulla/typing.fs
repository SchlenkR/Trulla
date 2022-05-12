module Trulla.Typing

open Parsing

type Tree =
    | LeafNode of LeafToken
    | InternalNode of root: ScopeToken * children: Tree list
and Token =
    | LeafToken of LeafToken
    | ScopeToken of ScopeToken
and LeafToken =
    | Text of string
    | Hole of PVal<AccessExp>
and ScopeToken =
    | For of ident: PVal<string> * exp: PVal<AccessExp>
    | If of PVal<AccessExp>

type Type =
    | Mono of MonoTyp
    | Poly of PolyTyp
and MonoTyp =
    | Bool
    | String
    | Record of RecordDef
and PolyTyp =
    | List of MonoTyp
and RecordDef = { name: string; fields: FieldDef list }
and FieldDef = { name: string; type': Type }

// TODO: meaningful error messages + location
// TODO: Performance?
let toTree (tokens: ParseResult) : Tree list =
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

let resolveType (symName: string) (boundSymbols: List<string * AccessExp>) =
    let rec tryResolve symName boundSymbols =
        printfn $"Searching for {symName} in {boundSymbols}"
        match boundSymbols with
        | [] -> None
        | (ident,source) :: boundSymbols ->
            if ident = symName then
                tryResolve source.ident boundSymbols
                |> Option.map (fun resolvedPath -> resolvedPath @ source.propPath)
                |> Option.defaultValue (source.ident :: source.propPath)
                |> Some
            else
                tryResolve symName boundSymbols
    tryResolve symName boundSymbols
    |> Option.defaultValue [symName]

let rec typeTree (trees: Tree list) (boundSymbols: List<string * AccessExp>) =
    [ for tree in trees do
        match tree with
        | LeafNode (Text _) -> ()
        | LeafNode (Hole hole as token) ->
            yield LeafToken token, resolveType hole.value.ident boundSymbols
        | InternalNode (For (ident,source) as token, children) ->
            yield ScopeToken token, resolveType ident.value boundSymbols
            let boundSymbols = (ident.value, source.value) :: boundSymbols
            yield! typeTree children boundSymbols
        | InternalNode (If ident as token, children) ->
            yield ScopeToken token, resolveType ident.value.ident boundSymbols
            yield! typeTree children boundSymbols
    ]
