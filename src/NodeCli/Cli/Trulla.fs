module Trulla

open Fable.Core
open Trulla
open Trulla.Core
open Node.Api

let proc = ``process``
let args = proc.argv |> Seq.skip 2
let workingDir = proc.cwd()

type Ctx = { inDir: string; outDir: string }

let ctx =
    let rec loop rest ctx =
        match rest with
        | "-i" :: dir :: rest -> loop rest { ctx with inDir = path.resolve(dir) }
        | "-o" :: dir :: rest -> loop rest { ctx with outDir = path.resolve(dir) }
        | _ :: rest -> failwith "use: trulla -i <input dir> -o <output dir>"
        | [] -> ctx
    loop (args |> List.ofSeq) { inDir = workingDir; outDir = workingDir }

printfn $"Working dir: {workingDir}"
printfn $"Ctx: {ctx}"

let renderTemplate template =
    match Solver.solve template with
    | Ok solution -> NodeCli.Renderer.renderTemplate solution
    | Error errors -> NodeCli.Renderer.renderErrors errors

let rec getAllFiles dir =
    let files = fs.readdirSync (U2.Case1 dir)
    [ 
        for file in files do
            let x = path.join(dir, "/", file)
            if fs.statSync(U2.Case1 x).isDirectory() 
            then yield! getAllFiles x
            else yield x
    ]

let trullaFileEnding = ".trulla"

let templates =
    getAllFiles ctx.inDir
    |> List.filter (fun f -> f.EndsWith(trullaFileEnding))

printfn $"Templates found: {templates.Length}"

let rec createDirectoryRecursive dirPath =
    let dir = path.dirname(dirPath)
    if not (fs.existsSync(U2.Case1 dir)) then
        createDirectoryRecursive dir

    if not (fs.existsSync(U2.Case1 dirPath)) then
        fs.mkdirSync(dirPath)

for inFilePath in templates do
    let outFileName =
        let inFilePath = inFilePath.Substring(0, inFilePath.Length - trullaFileEnding.Length)
        let inFilePath = inFilePath + ".ts"
        let inFilePath = inFilePath.Substring(ctx.inDir.Length)
        path.join(ctx.outDir, inFilePath)
    let outFilePath = path.dirname(outFileName)
    
    printfn $"Rendering template: {inFilePath} -> {outFileName}"
    let templateContent = fs.readFileSync(inFilePath).ToString() |> renderTemplate
    
    do createDirectoryRecursive outFilePath
    do fs.writeFileSync(outFileName, templateContent)
