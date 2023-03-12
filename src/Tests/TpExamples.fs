module TypedTemplateProvider.TpExamples

open System
open System.Reflection
open FSharp.Text.TypedTemplateProvider.DesignTime.Internal
open NUnit.Framework

module Helper =

    let rec renderTypeDef (t: Type) : string =
        let rec printTypeName (t: Type) : string =
            let genArgSuffix =
                match t.GenericTypeArguments with
                | [||] -> ""
                | args ->
                    let args = [ for ta in args do printTypeName ta ] |> String.concat ", "
                    $"<{args}>"
            $"{t.Name}{genArgSuffix}"
        let types = t.GetNestedTypes()
        [
            for t in types do
                let fields = 
                    [
                        let ctorParams = 
                            t.GetConstructor([||]) 
                            |> Option.ofObj
                            |> Option.map  (fun x -> x.GetParameters())
                            |> Option.defaultValue [||]
                        for p in ctorParams do
                            $"{p.Name}: {printTypeName p.ParameterType}"
                    ]
                    |> String.concat ", "
                $"{printTypeName t}({fields})"
        ]
        |> String.concat "\n"

    let createRootTypeDef template =
        let res = ProviderCompiler.createTypeDefForStringLiteral "Tmpl" template
        printfn "%s" (renderTypeDef res)
        ()


// -----------------------------------
// -----------------------------------


let [<TestCase>] ``Constant``() =
    Helper.createRootTypeDef RenderExamples.Constant.template
    ()

let [<TestCase>] ``Issue 8``() =
    let res = Helper.createRootTypeDef RenderExamples.``Issue 8``.template
    ()

let [<TestCase>] ``Issue 8_1``() =
    let res = Helper.createRootTypeDef RenderExamples.``Issue 8_1``.template
    ()
