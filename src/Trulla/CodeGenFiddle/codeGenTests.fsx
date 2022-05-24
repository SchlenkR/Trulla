
#r "nuget: FParsec, 1.1.1"

#load "../helper.fs"
#load "../parsing.fs"
#load "../typing.fs"
#load "../codeGen.FSharp.fs"

open Trulla.Internal.Parsing
open Trulla.Internal.Typing
open Trulla.Internal.CodeGen.FSharp

let printResult =
    function
    | Ok res -> printfn "%A" res
    | Error err -> failwithf "ERROR: %A" err

"""
Hello
{{if customer.isActive}}
ACTIVE
{{end}}
"""
|> render
|> printResult
