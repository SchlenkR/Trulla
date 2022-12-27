module TypedTemplateProvider.Misc

open FsUnit
open NUnit.Framework

let [<TestCase>] ``Constant``() =
    let template = "T"
    let model = obj()
    let result = Trulla.Rendering.reflectionRender model template
    result |> should equal template


let [<TestCase>] ``Simple Hole``() =
    let template = "Today is {{day}}."
    let model = 
        {|
            day = "Sunday"
        |}
    let result = Trulla.Rendering.reflectionRender model template
    result |> should equal "Today is Sunday."

