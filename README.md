# TypedTemplateProvider (aka "Trulla")

...an F# strongly typed text template provider!

> Status: Still experimental

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

## Example

```fsharp
#r "nuget: FSharp.Text.TypedTemplateProvider, 0.0.0-alpha01"

open FSharp.Text.TypedTemplateProvider

let [<Literal>] TestTemplate = """
    Hello {{user.name}}, how are you?

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
```

This will print:

```
    Hello Hans, how are you?

    Your Orders
    ---
    ID: Order 1

    ID: Order 2
    ORDER IS ACTIVE
```
