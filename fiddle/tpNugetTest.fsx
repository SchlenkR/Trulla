#r "nuget: FSharp.Text.TypedTemplateProvider, 0.0.0-alpha01"

open FSharp.Text.TypedTemplateProvider

let [<Literal>] TestTemplate =
    """Hello {{user.name}}, how are you?

Your Orders
---
{{for order in orders}}ID: {{order.id}}
{{if order.isActive}}ORDER IS ACTIVE{{end}}
{{end}}
"""

type Tmpl = Template<TestTemplate>

let root =
    Tmpl.Root(
        [
            Tmpl.order(false, "Order 1")
            Tmpl.order(true, "Order 2")
        ],
        Tmpl.user("Hans"))

Tmpl.Render(root) |> printfn "%s"

// this will print:

(*
Hello Hans, how are you?

Your Orders
---
ID: Order 1

ID: Order 2
ORDER IS ACTIVE
*)
