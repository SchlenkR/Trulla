namespace Trulla.Core.Utils

module List =
    let partitionMap3 (mapping: 'a -> Choice<'b,'c,'d>) (source: list<'a>) =
        let rec loop ((acc1, acc2, acc3) as acc) =
            function
            | [] -> acc
            | x::xs ->
                match mapping x with
                | Choice1Of3 x -> loop (x::acc1, acc2, acc3) xs
                | Choice2Of3 x -> loop (acc1, x::acc2, acc3) xs
                | Choice3Of3 x -> loop (acc1, acc2, x::acc3) xs
        loop ([], [], []) (List.rev source)

module Reflection =
    open Microsoft.FSharp.Reflection

    /// Returns the case name of the object with union type 'a.
    let getUnionCaseName (x:'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with case, _ -> case.Name

module Map =
    let find key errorContext map =
        map
        |> Map.tryFind key
        |> Option.defaultWith (fun () -> failwithf "Key not found: %A (context: %A)" key errorContext)

module String =
    let assertLetterDigitUnderscore (contextualMeaning) (s: string) =
        if 
            s.ToCharArray()
            |> Seq.exists (fun c -> not (System.Char.IsLetterOrDigit c || c = '_')) 
        then
            failwithf "The %s value '%s' is not a valid namespace value." contextualMeaning s
