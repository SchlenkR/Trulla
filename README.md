# TypedTemplateProvider (aka "Trulla")

...an F# strongly typed text template provider!

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
