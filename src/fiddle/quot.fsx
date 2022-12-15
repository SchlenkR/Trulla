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




open Microsoft.FSharp.Quotations

let varX = Var("x", typeof<string>)
let a = Expr.Let(varX, Expr.Value("Hello"), Expr.Var(varX))

let varExpr = Expr.Var(Var("x", typeof<string>))
let b = <@@let x = "Hello" in (%%varExpr: string)@@>



<@@ 
    [
        for x in [1] do
            x + 1
    ]
@@>

<@@ 
    List.map (fun x -> x + 1) [1]
@@>
