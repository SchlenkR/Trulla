# Trulla Templates

...it's like Handlebars or Mustache templates, but statically typed!

> Status: Stable

C# Source Generator: [![NuGet Badge](http://img.shields.io/nuget/v/Trulla.svg?style=flat)](https://www.nuget.org/packages/Trulla)

F# Type Provider: [![NuGet Badge](http://img.shields.io/nuget/v/Trulla.SourceGenerator.svg?style=flat)](https://www.nuget.org/packages/Trulla.SourceGenerator)


The Trulla Approcah
---

* Provide a text template with:
  * template parameters
  * iterations
  * conditionals
* A model type is infered from a given template.
* An instance of the model is provided by the user for rendering the final template.


Examples and Usage
---

* C# usage via Source Generators: Go to the [C# Documentation](./src/docs/SourceGenerator.md)
* F# usage via Type Providers: Go to the [F# Documentation](./src/docs/TypeProvider.md)


General Template Syntax
---

> Have a look at the [tests](./src/TypeProvider/Tests/RenderExamples.fs) for more samples!

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


## How it works internals

Trulla is implemented in F#. It basically contains everything a "real" language has, like 

* a parser
* an untyped AST
* type inference with a solver
* a typed AST
* code generation

If you want to know more, have a look at the (Internals)[./src/docs/Internals.md].
