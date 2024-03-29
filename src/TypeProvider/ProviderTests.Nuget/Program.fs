﻿module Program

open Trulla

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
    ===

    {{for order in orders}}ID: {{order.id}}
    ({{if order.isActive}}active{{else}}inactive{{end}})
    ---
    {{end}}
    """

    type Tmpl = Template<TestTemplate>

    let root =
        Tmpl.Root(
            [
                Tmpl.order("Order 1", false)
                Tmpl.order("Order 2", true)
            ],
            Tmpl.user("Hans"))
    let output = Tmpl.Render(root)

module Issue_7_Var =
    let [<Literal>] TestTemplate =
        """
            {{for a in as}} {{end}}
            {{for b in bs}} {{end}}
        """

    type Tmpl = Template<TestTemplate>

    let root =
        Tmpl.Root([], [])
    let output = Tmpl.Render(root)

module Issue8 =
    let [<Literal>] TestTemplate =
        """
            {{for x in as}}{{x.name}}{{end}}
            {{for x in bs}}{{x.name}}{{end}}
        """

    type Tmpl = Template<TestTemplate>

    let root =
        Tmpl.Root(
            [
                Tmpl.x1("a-name")
            ],
            [
                Tmpl.x("b-name")
            ])
    let output = Tmpl.Render(root)


module Issue8_1 =
    let [<Literal>] TestTemplate =
        """
            {{for x in as}}{{x.name}}{{end}}
            {{for x in bs}}{{x.name}}; {{x.name1}}{{end}}
        """

    type Tmpl = Template<TestTemplate>

    let root =
        Tmpl.Root(
            [
                Tmpl.x1("a-name")
            ],
            [
                Tmpl.x("b-name", "b-name1")
            ])
    let output = Tmpl.Render(root)


module Issue8_2 =
    let [<Literal>] TestTemplate =
        """
            {{for t in enumTypes}}
            type {{t.name}} = {{for l in t.labels}}
            | {{l.name}} {{end}}
            with override this.ToString() =
                match this with {{for l in t.labels}}
                | {{l.name}} -> "{{l.value}}" {{end}}
            {{end}}
        """
    
    type Tmpl = Template<TestTemplate>
    


        
[<EntryPoint>]
let main _ =
    printfn "%s" TextOnly.output
    printfn "%s" ScalarHole.output
    printfn "%s" ForLoop.output
    0
    
    