# TypedTemplateProvider (aka "Trulla")

...an F# strongly typed text template provider!

> Status: Still experimental

[![NuGet Badge](http://img.shields.io/nuget/v/FSharp.Text.TypedTemplateProvider.svg?style=flat)](https://www.nuget.org/packages/FSharp.Text.TypedTemplateProvider)

```fsharp
#r "nuget: FSharp.Text.TypedTemplateProvider, 0.0.0-alpha01"

open FSharp.Text.TypedTemplateProvider

let [<Literal>] TestTemplate = """
    Hello {{user.name}}, how are you?

    Your Orders
    ===

    {{for order in orders}}ID: {{order.id}}
    ({{if order.isActive}}active{{else}}inactive{{end}})
    ---
    {{end}}
    """

// All types required by the given template
// are infered and provided:
type Tmpl = Template<TestTemplate>

// Instanciate a typed model for the tempalte.
let templateModel =
    Tmpl.Root(
        [
            Tmpl.order(false, "Order 1")
            Tmpl.order(true, "Order 2")
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
```

The approach of FSharp.Text.TypedTemplateProvider is:
* Provide a text template with:
  * template parameters
  * iterations
  * conditionals
* A model type is infered from a given template.
* An instance of the model is provided by the user for rendering the final template.

## Limitations (current)

* The model will only contain  fields of type
  * list
  * string (for template holes)
  * bool 
* There are currently no partials supported

## TODOs

* Shadowing (Explanation)
* Wildcards in bindings
* The begin and end character sequence for template expressions are configurable, and there is no way escaping them. Choose an appropriate sequence of characters that won't occur in your template.

