//// For more information see https://aka.ms/fsharp-console-apps
//printfn "Hello from F#"

module Program

type Tmpl = Trulla.Template<2>

[<EntryPoint>]
let main _ =
    let root =
        Tmpl.Record2(
            "Hurz",
            Tmpl.Record1("xxx", "yyy"))
    let output = Tmpl.Render(root)
    printfn "%A" t
    0

