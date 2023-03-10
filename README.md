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

## Implementation Notes

The implementation of the tempalte provider might be interesting, because it contains (in a simple form) the building blocks that are required for a programming language. It has:

**A parser** [Parsing.fs](src/FSharp.Text.TypedTemplateProvider/Parsing.fs) implemented with FParsec. The parser output is a sequence of tokens:

```fsharp
type Token =
    | Text of string
    | Hole of PVal<MemberToken>
    | For of ident: PVal<string> * exp: PVal<MemberToken>
    | If of PVal<MemberToken>
    | Else
    | End
    
and MemberToken =
    | AccessToken of {| instanceExp: PVal<MemberToken>; memberName: string |}
    | IdentToken of string
```

**An untyped AST** [Ast.fs](src/FSharp.Text.TypedTemplateProvider/Ast.fs) that gets constructed from the parsed token sequence:

```fsharp

type TVar =
    | Root
    | TVar of int

type private BindingContext = Map<string, TVar>

type TVal<'a> =
    { 
        range: Range
        tvar: TVar
        bindingContext: BindingContext
        value: 'a 
    }
    override this.ToString() = sprintf "(%A)%A" this.range this.value

type TExp =
    | Text of string
    | Hole of TVal<MemberExp>
    | For of ident: TVal<string> * exp: TVal<MemberExp> * body: TExp list
    | If of cond: TVal<MemberExp> * body: TExp list
    | Else of cond: TVal<MemberExp> * body: TExp list

and Body = BindingContext * TExp list

and MemberExp =
    | AccessExp of {| instanceExp: TVal<MemberExp>; memberName: string |}
    | IdentExp of string

type Typ =
    | Mono of string
    | Poly of name: string * typParam: Typ
    | Field of Field
    | Record of TVar
    | Var of TVar

and Field = 
    { 
        name: string
        typ: Typ
    }
```

**A solver** [Solver.fs](src/FSharp.Text.TypedTemplateProvider/Solver.fs) that types records and identifiers of the AST

```fsharp
type RecordDef =
    {
        id: TVar
        fields: Field list
        name: string
    }
```

**A generator (renderer)** [Rendering.fs](src/FSharp.Text.TypedTemplateProvider/Rendering.fs) that transforms all the previous into the final string.

## TODOs

* Shadowing (Explanation)
* Wildcards in bindings
* The begin and end character sequence for template expressions are configurable, and there is no way escaping them. Choose an appropriate sequence of characters that won't occur in your template.
