<div align="center">
   <h2>🎁 <strong>Win a PXL-Clock - Watch How on YouTube</strong> 🎁</h2>
   <a href="https://youtu.be/q5-QTpEMGdU"><img src="https://img.youtube.com/vi/q5-QTpEMGdU/0.jpg" alt="Watch the PXL-JAM video" style="width: 60%;"></a>
   <p>YouTube</p>
</div>

The PXL PAM 2024 is a fun and engaging way to come together and have a joyful time!
Watch the video or check out our [PXL-JAM repo](https://github.com/CuminAndPotato/PXL-JAM/)!

---

<p align="center">
    <img src='./artwork/Original Logo.png' alt='logo' width='300' />
</p>

Trulla Templates is like Handlebars or Mustache templates, but statically typed and safe!

[![NuGet Badge](http://img.shields.io/nuget/v/Trulla.svg?style=flat)](https://www.nuget.org/packages/Trulla) - **C# Source Generator**

[![NuGet Badge](http://img.shields.io/nuget/v/Trulla.SourceGenerator.svg?style=flat)](https://www.nuget.org/packages/Trulla.SourceGenerator) - **F# Type Provider**

[![npm version](https://img.shields.io/npm/v/trulla-templates.svg?style=flat)](https://badge.fury.io/js/trulla-templates) - **Node CLI**

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

**Node via CLI**

Go to the [Node CLI Documentation](./src/docs/NodeCli.md)

**C# usage via Source Generators**

Go to the [C# Documentation](./src/docs/SourceGenerator.md)

**F# usage via Type Providers****

Go to the [F# Documentation](./src/docs/TypeProvider.md)


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


## How it works internally

Trulla is implemented in F#. It basically contains everything a "real" language has, like 

* a parser
* an untyped AST
* type inference with a solver
* a typed AST
* code generation

If you want to know more, have a look at the (Internals)[./src/docs/Internals.md].
