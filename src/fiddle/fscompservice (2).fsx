#r "nuget: FSharp.Compiler.Service, 42.7.101"

open System.IO
open FSharp.Compiler.CodeAnalysis

// Create an interactive checker instance 
let checker = FSharpChecker.Create()

let fn = Path.GetTempFileName()
let fn2 = Path.ChangeExtension(fn, ".fsx")
let fn3 = Path.ChangeExtension(fn, ".dll")

File.WriteAllText(fn2, """
module M

type C() = 
   member x.P = 1

let x = 3 + 4
""")

let errors1, exitCode1 = 
    checker.Compile([| "fsc.exe"; "-o"; fn3; "-a"; fn2 |]) 
    |> Async.RunSynchronously

