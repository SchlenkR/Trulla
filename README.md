# Trulla

Text template model type provider written in F#.

The approach of Trulla is:
* Provide a text template with
  * template parameters
  * iterations
  * conditionals
* Trulla infers a model type for the template
* An instance of the model is provided by the user for rendering the final template.
* The begin and end character sequence for template expressions are configurable, and there is no way escaping them. Choose an appropriate sequence of characters that won't occur in your template.

Limitations

* The model will only contain  fields of type
  * list
  * string (for template holes)
  * bool 
* There are currently no partials supported

TODO
* Shadowing (Explanation)
* Wildcards in bindings
