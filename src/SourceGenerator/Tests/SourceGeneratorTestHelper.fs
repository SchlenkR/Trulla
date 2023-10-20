module Trulla.SourceGenerator.Tests.TestHelper

let testTemplate template =
    let solution = Trulla.Core.Solver.solve template
    match solution with
    | Error errors -> failwithf "Template error: %A" errors
    | Ok solution -> 
        let csharpCode = Trulla.SourceGenerator.Renderer.renderTemplate solution "TestNamespace"
        do printfn "%s" csharpCode
        ()
