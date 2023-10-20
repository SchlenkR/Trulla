module Trulla

open Fable.Core
open System
open System.IO
open Trulla
open Trulla.Core
open Node.Api

let proc = ``process``
let args = proc.argv |> Seq.skip 2

let renderTemplate template =
    let finalContent = 
        match Solver.solve template with
        | Ok solution ->
            NodeCli.Renderer.renderTemplate solution "Trulla"
        | Error errors ->
            NodeCli.Renderer.renderErrors errors
    printfn "%s" finalContent

let workingDir = proc.cwd()
printfn $"Working dir: {workingDir}"

let rec getAllFiles dir =
    let files = fs.readdirSync (U2.Case1 dir)
    [ 
        for file in files do
            let x = path.join(dir, "/", file)
            if fs.statSync(U2.Case1 x).isDirectory() 
            then yield! getAllFiles x
            else yield x
    ]

let templates =
    getAllFiles workingDir
    |> List.filter (fun f -> f.EndsWith(".trulla"))

for template in templates do
    printfn $"Rendering template: {template}"
    let templateContent = fs.readFileSync(template).ToString()
    renderTemplate templateContent
    printfn $"Done rendering template: {template}"
