module Trulla.Internal.Helper

type ResultBuilder() =
    member this.Bind(m, f) = Result.bind f m
    member this.Return(value) = Ok value
let result = ResultBuilder()

let partitionMap (mapping: 'T -> Choice<'T1,'T2>) (source: list<'T>) =
    let rec loop ((acc1, acc2) as acc) =
        function
        | [] -> acc
        | x::xs ->
            match mapping x with
            | Choice1Of2 x -> loop (x::acc1, acc2) xs
            | Choice2Of2 x -> loop (acc1, x::acc2) xs
    loop ([], []) (List.rev source)

open Microsoft.FSharp.Reflection

/// Returns the case name of the object with union type 'a.
let getUnionCaseName (x:'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with case, _ -> case.Name
