
#r "nuget: FParsec, 1.1.1"

#load "../helper.fs"
#load "../parsing.fs"
#load "../typing.fs"
#load "../codeGen.FSharp.fs"

open Trulla.Internal.Parsing
open Trulla.Internal.Typing
open Trulla.Internal.CodeGen.FSharp

let printResult renderRes =
    let printLines() = printfn "---------------------------------"

    do printLines()
    match renderRes with
    | Ok res -> printfn "%A" res
    | Error err -> failwithf "ERROR: %A" err
    do printLines()

"""
Hello
{{if customer.isActive}}
ACTIVE
{{for order in customer.orders}}
Order ID: {{x.id}}
{{end}}
{{end}}
"""
|> render
|> printResult
