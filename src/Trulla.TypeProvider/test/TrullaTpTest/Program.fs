//// For more information see https://aka.ms/fsharp-console-apps
//printfn "Hello from F#"

module Program

type T = Trulla.TrullaProvider<8>

[<EntryPoint>]
let main _ =
    let t = T().Property1
    printfn "%A" t
    0

