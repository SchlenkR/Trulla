module Trulla.TypeProviderImplementation.CompilerService

open System.IO
open FSharp.Compiler.CodeAnalysis

let compile code =
    let checker = FSharpChecker.Create()

    let fn = Path.GetTempFileName()
    let fnIn = Path.ChangeExtension(fn, ".fs")
    let fnOut = Path.ChangeExtension(fn, ".dll")

    File.WriteAllText(fnIn, code)

    let errors, exitCode =
        checker.Compile([| "fsc.exe"; "-o"; fnOut; "-a"; fnIn |]) 
        |> Async.RunSynchronously

    if exitCode <> 0 then
        failwith $"Error in render compilation: {errors}"

    let content = File.ReadAllBytes(fnOut)
    do
        File.Delete(fnIn)
        File.Delete(fnOut)
    content
