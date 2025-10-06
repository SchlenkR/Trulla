module Trulla.Tests.RenderExamples

open Trulla.Tests.TestHelper
open NUnit.Framework

module ``Constant`` =
    let model = obj()
    let [<TestCase>] test() = test Templates.Constant model Templates.Constant


module ``Simple Hole``=
    let model = 
        {|
            day = "Sunday"
        |}
    let expected = "Sunday"
    let [<TestCase>] test() = test Templates.``Simple Hole`` model expected


module ``Simple for`` =
    let model = 
        {|
            numbers = [1;2;3] |> List.map string
        |}
    let expected = "123"
    let [<TestCase>] test() = test Templates.``Simple for`` model expected


module ``For with separator`` =
    let model = 
        {|
            numbers = [1;2;3] |> List.map string
        |}
    let expected = "1 , 2 , 3"
    let [<TestCase>] test() = test Templates.``For with separator`` model expected


module ``For with empty separator`` =
    let model = 
        {|
            numbers = [1;2;3] |> List.map string
        |}
    let expected = "123"
    let [<TestCase>] test() = test Templates.``For with empty separator`` model expected


module ``Simple if`` =
    let model = 
        {|
            x = true
            y = false
        |}
    let expected = "X"
    let [<TestCase>] test() = test Templates.``Simple if`` model expected


module ``Simple if else`` =
    let model = 
        {|
            x = false
        |}
    let expected = "Else-Branch"
    let [<TestCase>] test() = test Templates.``Simple if else`` model expected


module ``Free vars and Issue 7 Var(n)`` =
    let model = {| ``as`` = ([] : obj list); bs = ([] : obj list) |}
    let extecped = ""
    let [<TestCase>] test() = test Templates.``Free vars and Issue 7 Var(n)`` model extecped


module ``Issue 8`` =
    let model = {| ``as`` = [ {| name = "A" |} ]; bs = [ {| name = "B" |} ] |}
    let expected = "AB"
    let [<TestCase>] test() = test Templates.``Issue 8`` model expected


module ``Issue 8_1`` =
    let model = {| ``as`` = [ {| name = "A" |} ]; bs = [ {| name = "B1"; name1 = "B1'" |} ] |}
    let expected = "AB1B1'"
    let [<TestCase>] test() = test Templates.``Issue 8_1`` model expected


module ``Issue 8_2`` =
    let model = {| ``a`` = [ {| name1 = "1"; name2 = "2" |} ] |}
    let expected = "12"
    let [<TestCase>] test() = test Templates.``Issue 8_2`` model expected
