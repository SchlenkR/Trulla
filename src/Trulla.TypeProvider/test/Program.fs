#if INTERACTIVE
#else
module Program
#endif



let [<Literal>] TestTemplate1 =
    """Hello {{user.name}}, how are you?

Your Orders
---
{{for order in orders}}ID: {{order.id}}
{{if order.isActive}}ORDER IS ACTIVE{{end}}
{{end}}
"""

let [<Literal>] TestTemplate2 = """Hello {{xxx}} Hans"""


type Tmpl = Trulla.Template<TestTemplate1 >

let root =
    //Tmpl.Root("MY-NAME-IS")
    Tmpl.Root(
        [
            Tmpl.order(false, "Order 1")
            Tmpl.order(true, "Order 2")
        ],
        Tmpl.user("Hans im Glück"))
let output = Tmpl.Render(root)



#if INTERACTIVE
#else
[<EntryPoint>]
let main _ =
    printfn "%A" output
    0
#endif
