module Trulla.Tests.CSharpCodeGen

open FsUnit
open NUnit.Framework

module ``Demo`` =
    let template = "T"
    let model = obj()
    let [<TestCase>] test() =
        let template = """
Hello {{user.name}}, how are you?

Your Orders
===

{{for order in orders|---}}
ID: {{order.id}}
({{if order.isActive}}active{{else}}inactive{{end}})
{{end}}
            """
        
        let solution = Trulla.Core.Solver.solve template
        match solution with
        | Error errors -> failwithf "Template error: %A" errors
        | Ok solution -> 
            let csharpCode = Trulla.SourceGenerator.Renderer.renderTemplate solution
            do printfn "%s" csharpCode
            ()
