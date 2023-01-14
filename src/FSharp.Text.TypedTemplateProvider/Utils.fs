namespace Trulla.Core.Utils

[<AutoOpen>]
module Result =
    type ResultBuilder() =
        member _.Bind(m, f) = Result.bind f m
        member _.Return(value) = Ok value
    let result = ResultBuilder()

[<AutoOpen>]
module List =
    let partitionMap (mapping: 'a -> Choice<'b,'c>) (source: list<'a>) =
        let rec loop ((acc1, acc2) as acc) =
            function
            | [] -> acc
            | x::xs ->
                match mapping x with
                | Choice1Of2 x -> loop (x::acc1, acc2) xs
                | Choice2Of2 x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)

[<AutoOpen>]
module Reflection =
    open Microsoft.FSharp.Reflection

    /// Returns the case name of the object with union type 'a.
    let getUnionCaseName (x:'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with case, _ -> case.Name
