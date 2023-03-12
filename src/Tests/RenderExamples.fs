module TypedTemplateProvider.RenderExamples

open FsUnit
open NUnit.Framework

let private test template model expected =
    let result = Trulla.Rendering.reflectionRender model template
    result |> should equal expected


// -----------------------------------
// -----------------------------------


let [<TestCase>] ``Constant``() =
    let template = "T"
    let model = obj()
    test template model template


let [<TestCase>] ``Simple Hole``() =
    let template = "{{day}}"
    let model = 
        {|
            day = "Sunday"
        |}
    let expected = "Sunday"
    test template model expected


let [<TestCase>] ``Simple for``() =
    let template = "{{for x in numbers}}{{x}}{{end}}"
    let model = 
        {|
            numbers = [1;2;3] |> List.map string
        |}
    let expected = "123"
    test template model expected


let [<TestCase>] ``Simple if``() =
    let template = "{{if x}}X{{end}}{{if y}}Y{{end}}"
    let model = 
        {|
            x = true
            y = false
        |}
    let expected = "X"
    test template model expected


let [<TestCase>] ``Simple if else``() =
    let template = "{{if x}}If-Branch{{else}}Else-Branch{{end}}"
    let model = 
        {|
            x = false
        |}
    let expected = "Else-Branch"
    test template model expected

let [<TestCase>] ``Free vars and Issue 7 Var(n)``() =
    let template = 
        """{{for a in as}}{{end}}{{for b in bs}}{{end}}"""
    let res = Trulla.Solver.solve template
    match res with
    | Ok _ -> ()
    | Error err -> Assert.Fail $"Unexpected error while solving template: {err}"

    test template {| ``as`` = []; bs = [] |} ""

let [<TestCase>] ``Issue 8``() =
    let template = """{{for a in as}}{{a.nameA}}{{end}}{{for b in bs}}{{b.nameB}}{{end}}"""
    let res = Trulla.Solver.solve template
    match res with
    | Ok _ -> ()
    | Error err -> Assert.Fail $"Unexpected error while solving template: {err}"

    test template {| ``as`` = [ {| nameA = "A" |} ]; bs = [ {| nameB = "B" |} ] |} "AB"
