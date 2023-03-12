module TypedTemplateProvider.RenderExamples

open FsUnit
open NUnit.Framework

let private test template model expected =
    let result = Trulla.Rendering.reflectionRender model template
    result |> should equal expected


// -----------------------------------
// -----------------------------------


module ``Constant`` =
    let template = "T"
    let model = obj()
    let [<TestCase>] test() = test template model template


module ``Simple Hole``=
    let template = "{{day}}"
    let model = 
        {|
            day = "Sunday"
        |}
    let expected = "Sunday"
    let [<TestCase>] test() = test template model expected


module ``Simple for`` =
    let template = "{{for x in numbers}}{{x}}{{end}}"
    let model = 
        {|
            numbers = [1;2;3] |> List.map string
        |}
    let expected = "123"
    let [<TestCase>] test() = test template model expected


module ``Simple if`` =
    let template = "{{if x}}X{{end}}{{if y}}Y{{end}}"
    let model = 
        {|
            x = true
            y = false
        |}
    let expected = "X"
    let [<TestCase>] test() = test template model expected


module ``Simple if else`` =
    let template = "{{if x}}If-Branch{{else}}Else-Branch{{end}}"
    let model = 
        {|
            x = false
        |}
    let expected = "Else-Branch"
    let [<TestCase>] test() = test template model expected


module ``Free vars and Issue 7 Var(n)`` =
    let template = 
        """{{for a in as}}{{end}}{{for b in bs}}{{end}}"""
    let res = Trulla.Solver.solve template
    do
        match res with
        | Ok _ -> ()
        | Error err -> Assert.Fail $"Unexpected error while solving template: {err}"
    let model = {| ``as`` = ([] : obj list); bs = ([] : obj list) |}
    let extecped = ""
    let [<TestCase>] test() = test template model extecped


module ``Issue 8`` =
    let template = """{{for a in as}}{{a.nameA}}{{end}}{{for b in bs}}{{b.nameB}}{{end}}"""
    let res = Trulla.Solver.solve template
    do
        match res with
        | Ok _ -> ()
        | Error err -> Assert.Fail $"Unexpected error while solving template: {err}"
    let model = {| ``as`` = [ {| nameA = "A" |} ]; bs = [ {| nameB = "B" |} ] |}
    let expected = "AB"
    let [<TestCase>] test() = test template model expected