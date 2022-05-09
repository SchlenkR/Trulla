module Trulla.Typing

////type UExpr =
////    | PropAccess of string * string list
////    | For of ident: string * e: UExpr * body: UExpr list
////    | If of cond: UExpr * trueBranch: UExpr list * falseBranch: UExpr list

////type Tree =
////    | Scope of ident: string * Tree list

type MonoTyp =
    | Bool
    | String
type PolyTyp =
    | List of MonoTyp
type Typ =
    | Mono of MonoTyp
    | Poly of PolyTyp

type GlobalSymbols = List<string * Typ>

type SymbolBindingsContext = Map<string, string>

open Trulla.Parsing

// TODO: meaningful error messages + location
let rec toTree (tokens: Token list) (isOpened: bool) =
    [
        for token in tokens do
            match token with
            | Text text -> text
            | 
    ]
    