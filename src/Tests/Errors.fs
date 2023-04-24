module Trulla.Tests.Errors

open NUnit.Framework

let private test template model =
    try
        do Trulla.Rendering.reflectionRender model template |> ignore
        failwith "Exception expected, but no exception was thrown."
    with _ -> ()


let [<TestCase>] ``End without opened scope``() =
    let template = "{{end}}"
    let model = obj()
    test template model


let [<TestCase>] ``Unfinished opened scope``() =
    let template = "{{if x}}X"
    let model = 
        {|
            x = true
        |}
    test template model



let [<TestCase>] ``Else without if``() =
    let template = "{{else}}X"
    let model = 
        {|
            x = true
        |}
    test template model

