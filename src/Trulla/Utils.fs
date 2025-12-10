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

module Map =
    let find key errorContext map =
        map
        |> Map.tryFind key
        |> Option.defaultWith (fun () -> failwithf "Key not found: %A (context: %A)" key errorContext)
