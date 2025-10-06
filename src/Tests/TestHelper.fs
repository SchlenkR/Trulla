module Trulla.Tests.TestHelper

open FsUnit

let test template model expected =
    let result = Trulla.Core.ReflectionRenderer.renderTemplate model template
    result |> should equal expected

module Templates =
    let ``Constant`` = 
        "T"
    let ``Simple Hole``= 
        "{{day}}"
    let ``Simple for`` = 
        "{{for x in numbers}}{{x}}{{end}}"
    let ``For with separator`` = 
        "{{for x in numbers| , }}{{x}}{{end}}"
    let ``For with empty separator`` = 
        "{{for x in numbers|}}{{x}}{{end}}"
    let ``Simple if`` = 
        "{{if x}}X{{end}}{{if y}}Y{{end}}"
    let ``Simple if else`` = 
        "{{if x}}If-Branch{{else}}Else-Branch{{end}}"
    let ``Free vars and Issue 7 Var(n)`` = 
        """{{for a in as}}{{end}}{{for b in bs}}{{end}}"""
    let ``Issue 8`` =
        """{{for x in as}}{{x.name}}{{end}}{{for x in bs}}{{x.name}}{{end}}"""
    let ``Issue 8_1`` =
        """{{for x in as}}{{x.name}}{{end}}{{for x in bs}}{{x.name}}{{x.name1}}{{end}}"""
    let ``Issue 8_2`` =
        """{{for x in a}}{{x.name1}}{{end}}{{for x in a}}{{x.name2}}{{end}}"""
