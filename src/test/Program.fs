module Program

type Tmpl = Trulla.Template

[<EntryPoint>]
let main _ =
    let output = Tmpl.Render()
    printfn "%A" output
    0
