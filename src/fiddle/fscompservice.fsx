#r "nuget: FSharp.Compiler.Service, 42.7.101"

open System.IO
open System.Threading
open FSharp.Compiler.Interactive.Shell

let mutable session : FsiEvaluationSession option = None

ThreadPool.QueueUserWorkItem(fun _ ->
    let defaultArgs =
        [| 
            "fsi.exe"
            "--noninteractive"
            "--nologo"
            "--gui-" 
        |]
    
    let inStream = new StringReader("")
    let outStream = new StringWriter()
    let errStream = new StringWriter()
    
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    
    session <- 
        FsiEvaluationSession.Create(fsiConfig, defaultArgs, inStream, outStream, errStream, collectible = true)
        |> Some
)


session |> Option.iter (fun session ->
    let v = session.EvalExpression("222")
    printfn "result = %A" v.Value.ReflectionValue
)

