module TypedTemplateProvider.TpExamples

open FsUnit
open NUnit.Framework
open FSharp.Text.TypedTemplateProvider.DesignTime.Internal


let [<TestCase>] ``Constant``() =
    let template = "T"
    let res =
        let typeName = "Tmpl"
        ProviderCompiler.createTypeDefForStringLiteral typeName template
    ()
