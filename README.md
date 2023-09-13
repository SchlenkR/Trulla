# Trulla Templates

...an F# strongly typed text template provider!

> Status: Stable

[![NuGet Badge](http://img.shields.io/nuget/v/Trulla.svg?style=flat)](https://www.nuget.org/packages/Trulla)

```fsharp
#r "nuget: Trulla"

open Trulla

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

The approach of Trulla is:

* Provide a text template with:
  * template parameters
  * iterations
  * conditionals
* A model type is infered from a given template.
* An instance of the model is provided by the user for rendering the final template.


**Limitations (current)**

* The model will only contain  fields of type
  * list
  * string (for template holes)
  * bool 
* There are currently no partials supported


## Template Syntax

> Have a look at the [tests](./src/Tests/RenderExamples.fs) for more samples!

**for loops (with separator)**

* This will render 'abc' for given chars = ["a"; "b"; "c"]:

```
{{for c in chars}}{{c}}{{end}}
```

* With a given separator between items, this will render 'a;b;c' for given chars = ["a"; "b"; "c"]:

```
{{for x in numbers|;}}{{x.id}}{{end}}
```

**if/else**

* This will print "Order is active." or "Order is closed." depending on the value of order.isActive:
* The `else` part is optional.

```
Order is {{if order.isActive}}active{{else}}closed{{end}}.
```


## Implementation Notes

The implementation of the tempalte provider might be interesting, because it contains (in a simple form) the building blocks that are required for a programming language. It has:

**A parser** [Parsing.fs](src/Trulla/Parsing.fs) implemented with FParsec. The parser output is a sequence of tokens:

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

**An untyped AST** [Ast.fs](src/Trulla/Ast.fs) that gets constructed from the parsed token sequence:

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

**A solver** [Solver.fs](src/Trulla/Solver.fs) that types records and identifiers of the AST

```fsharp
type RecordDef =
    {
        id: TVar
        fields: Field list
        name: string
    }
```

**A generator (renderer)** [Rendering.fs](src/Trulla/Rendering.fs) that transforms all the previous into the final string.

## TODOs

* Shadowing (Explanation)
* Wildcards in bindings
* The begin and end character sequence for template expressions are configurable, and there is no way escaping them. Choose an appropriate sequence of characters that won't occur in your template.
