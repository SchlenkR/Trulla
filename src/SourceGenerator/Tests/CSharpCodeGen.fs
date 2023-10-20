module Trulla.SourceGenerator.Tests.CSharpCodeGen

open NUnit.Framework

let [<TestCase>] ``Demo`` () =
    """
Hello {{user.name}}, how are you?

Your Orders
===

{{for order in orders|---}}
ID: {{order.id}}
({{if order.isActive}}active{{else}}inactive{{end}})
{{end}}
        """
        |> TestHelper.testTemplate


let [<TestCase>] ``Merge Records`` () =
    """
{{for order in orders|---}}
{{order.id}}
{{end}}
{{for order in orders|---}}
{{order.name}}
{{end}}
        """
        |> TestHelper.testTemplate
