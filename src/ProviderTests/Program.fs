module Program

open FSharp.Text.TypedTemplateProvider

module TextOnly =
    let [<Literal>] TestTemplate = """Hello, how are you?"""

    type Tmpl = Template<TestTemplate>

    let root = Tmpl.Root()
    let output = Tmpl.Render(root)

module ScalarHole =
    let [<Literal>] TestTemplate = """Hello {{userName}}, how are you?"""

    type Tmpl = Template<TestTemplate>

    let root = Tmpl.Root("Hans")
    let output = Tmpl.Render(root)

module ForLoop =
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
    let output = Tmpl.Render(root)


[<EntryPoint>]
let main _ =
    printfn "%s" TextOnly.output
    printfn "%s" ScalarHole.output
    printfn "%s" ForLoop.output
    0
