#r "nuget: QuotationCompiler"

open System.Text
open System.Reflection
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

let q = 
    <@
        let x (s: string) = ()
        x "Hello"
    @>

Expr.Var(Var("myVar", typeof<int>))


let appendHello = Expr.Application(Expr.Var(Var("append", typeof<string -> unit>)), Expr.Value("Hello World"))
<@@ 
    let sb = StringBuilder()
    let append (txt: string) = sb.Append(txt) |> ignore
    do (%%appendHello)
    sb.ToString()
@@>

<@@ 
    let sb = StringBuilder()
    let append (txt: string) = sb.Append(txt) |> ignore
    do append "Hello World"
    sb.ToString()
@@>
