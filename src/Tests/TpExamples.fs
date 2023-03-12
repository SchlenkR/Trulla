module TypedTemplateProvider.TpExamples

open System
open System.Reflection
open FSharp.Text.TypedTemplateProvider.DesignTime.Internal
open NUnit.Framework

module Helper =

    let rec renderTypeDef (indentDepth: int) (t: Type) : string =
        let indent = String.replicate indentDepth "    "
        
        let rec printTypeName (t: Type) : string =
            let genArgSuffix =
                match t.GenericTypeArguments with
                | [||] -> ""
                | args ->
                    let args = [ for ta in args do printTypeName ta ] |> String.concat ", "
                    $"<{args}>"
            $"{t.Name}{genArgSuffix}"
        
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

        [
            $"{indent}{printTypeName t}({fields})"
            for nestedT in t.GetNestedTypes() do
                renderTypeDef (indentDepth + 1) nestedT
        ]
        |> String.concat "\n"
        

    let createRootTypeDef template =
        let res = ProviderCompiler.createTypeDefForStringLiteral "Tmpl" template
        printfn "%s" (renderTypeDef 0 (res :> Type))
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
