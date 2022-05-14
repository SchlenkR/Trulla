
#r "nuget: FParsec"

#load "../parsing.fs"
open Trulla
open Trulla.Parsing




let rec tryResolveType (symName: string) (boundSymbols: List<string * AccessExp>) =
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


(*
Hello World
{{ for matchingContext in contexts }}
{{ for customer in matchingContext.collections }}
{{ for order in customer.masterData.orders }}
Order Nr.: {{order.number}}
{{ if order.isDispatched }}
	-> IS DISPATCHED!
{{ user.address.street }}

{{ end }}
{{ end }}
{{ end }}

*)
let boundSymbols =
    [
        "order", { ident = "customer"; propPath = ["masterData"; "orders"] }
        "customer", { ident = "matchingContext"; propPath = ["collections"] }
        "matchingContext", { ident = "contexts"; propPath = [] }
    ]

tryResolveType "order" boundSymbols
