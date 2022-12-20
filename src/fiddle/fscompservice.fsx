#r "nuget: FSharp.Compiler.Service, 42.7.101"

open System.IO
open System.Threading
open FSharp.Compiler.Interactive.Shell

let eval expr = ThreadPool.QueueUserWorkItem(fun _ ->
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
    
    let session =
        FsiEvaluationSession.Create(fsiConfig, defaultArgs, inStream, outStream, errStream, collectible = true)

    let res,diag = session.EvalExpressionNonThrowing(expr)
    match res with
    | Choice1Of2 v -> printfn $"result = {v.Value.ReflectionValue}"
    | Choice2Of2 err -> printfn $"Error: {err}"
)

eval "4444"
