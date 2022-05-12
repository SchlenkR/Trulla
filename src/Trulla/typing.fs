module Trulla.Typing

open Parsing

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

type SymbolicLinks = Map<Range, string list>

let symbolLinks (trees: Tree list) : SymbolicLinks =
    let rec symbolLinks (trees: Tree list) (boundSymbols: List<string * AccessExp>) =
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) -> ()
            | LeafNode (Hole hole) ->
                yield hole.range, resolveType hole.value.ident boundSymbols
            | InternalNode (For (ident,source), children) ->
                yield ident.range, resolveType ident.value boundSymbols
                let boundSymbols = (ident.value, source.value) :: boundSymbols
                yield! symbolLinks children boundSymbols
            | InternalNode (If ident, children) ->
                yield ident.range, resolveType ident.value.ident boundSymbols
                yield! symbolLinks children boundSymbols
        ]
    symbolLinks trees [] |> Map.ofList

type Type =
    | Mono of MonoTyp
    | Poly of PolyTyp
    | Var of TyVar
and MonoTyp =
    | Bool
    | String
    | Record of RecordDef
and PolyTyp =
    | Sequence of Type
and RecordDef = { name: string; fields: FieldDef list }
and FieldDef = { name: string; type': Type }
and TyVar = int

type X =
    | VarLink of Range * TyVar
    | TypeConstraint of TyVar * Type

let symbolTypes (trees: Tree list) =
    let newTyvar =
        let mutable x = -1
        fun () -> x <- x + 1; x
    let rec symbolTypes (trees: Tree list) =
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) -> ()
            | LeafNode (Hole hole) ->
                let var = newTyvar()
                yield VarLink (hole.range, var)
                yield TypeConstraint (var, Mono String)
            | InternalNode (For (ident,source), children) ->
                let varIdent = newTyvar()
                yield VarLink (ident.range, varIdent)
                yield TypeConstraint (varIdent, Var varIdent)
                let varSource = newTyvar()
                yield TypeConstraint (varSource, Poly (Sequence (Var varIdent)))

                yield! symbolTypes children
            | InternalNode (If ident, children) ->
                let var = newTyvar()
                yield VarLink (ident.range, var)
                yield TypeConstraint (var, Mono Bool)
                
                yield! symbolTypes children
        ]
    symbolTypes trees

//let symbolTypes (trees: Tree list) (symLinks: SymbolicLinks) =
//    let newTyvar =
//        let mutable x = -1
//        fun () -> x <- x + 1; x
//    let rec symbolTypes (trees: Tree list) =
//        let typeOf pval = Map.find pval.range symLinks
//        [ for tree in trees do
//            match tree with
//            | LeafNode (Text _) -> ()
//            | LeafNode (Hole hole) ->
//                yield typeOf hole, Mono String
//            | InternalNode (For (ident,source), children) ->
//                let var = newTyvar()
//                yield typeOf ident, Var var
//                yield typeOf source, Poly (Sequence (Var var))
//                yield! symbolTypes children
//            | InternalNode (If ident, children) ->
//                yield typeOf ident, Mono Bool
//                yield! symbolTypes children
//        ]
//    symbolTypes trees
