module Program

let [<Literal>] TestTemplate1 =
    """Hello {{user.name}}, how are you?

Your Orders
---
{{for order in orders}}ID: {{order.id}}
{{if order.isActive}}ORDER IS ACTIVE{{end}}
{{end}}
"""

let [<Literal>] TestTemplate2 = """Hello {{xxx}} Hans"""


type Tmpl = Trulla.Template<TestTemplate2>

[<EntryPoint>]
let main _ =
    let root =
        Tmpl.Root("MY-NAME-IS")
        //Tmpl.Root(
        //    [
        //        Tmpl.order(false, "Order 1")
        //        Tmpl.order(true, "Order 2")
        //    ],
        //    Tmpl.user("Hans im Glück"))
    let output = Tmpl.Render(root)
    printfn "%A" output
    0
