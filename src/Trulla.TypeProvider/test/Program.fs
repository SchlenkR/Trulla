module Program

let [<Literal>] TestTemplate =
    """Hello {{user.name}}, how are you?

Your Orders
---
{{for order in orders}}ID: {{order.id}}
{{if order.isActive}}ORDER IS ACTIVE{{end}}
{{end}}
"""


type Tmpl = Trulla.Template<TestTemplate>

[<EntryPoint>]
let main _ =
    let root =
        Tmpl.Root(
            [
                Tmpl.order(false, "Order 1")
                Tmpl.order(true, "Order 2")
            ],
            Tmpl.user("Hans im Glück"))
    let output = Tmpl.Render(root)
    printfn "%A" output
    0
