# Trulla Templates

[![NuGet Badge](http://img.shields.io/nuget/v/Trulla.svg?style=flat)](https://www.nuget.org/packages/Trulla)

```fsharp
#r "nuget: Trulla"

open Trulla

let [<Literal>] TestTemplate = """
Hello {{user.name}}, how are you?

Your Orders
===
{{for order in orders|---}}
ID: {{order.id}}
({{if order.isActive}}active{{else}}inactive{{end}})
{{end}}
    """

// All types required by the given template
// are infered and provided:
type Tmpl = Template<TestTemplate>

// Instanciate a typed model for the tempalte.
let templateModel =
    Tmpl.Root(
        [
            Tmpl.order("Order 1", false)
            Tmpl.order("Order 2", true)
            Tmpl.order("Order 3", false)
        ],
        Tmpl.user("Hans"))

// Render and print it:
Tmpl.Render(templateModel) |> printfn "%s"
```

This will print:

```
Hello Hans, how are you?

Your Orders
===

ID: Order 1
(inactive)
---
ID: Order 2
(active)
---
ID: Order 3
(inactive)
```
