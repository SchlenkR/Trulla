
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

#r "nuget: Fake.Core.Process"
#r "nuget: Fake.IO.FileSystem"

open System

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators

Trace.trace $"Starting script..."

let ( </> ) = Path.combine

module Properties =
    let nugetServer = "https://api.nuget.org/v3/index.json"
    let nugetPushEnvVarName = "nuget_push"

module Paths =
    //let slnRoot = "../src/TypeProvider"
    //let slnPath = "../src/TypeProvider/Trulla.sln"
    let packPath = __SOURCE_DIRECTORY__ </> "../.pack"
    
[<AutoOpen>]
module Helper =

    let private runTarget (x: string * _) =
        let name,f = x
        Trace.trace $"Running task: {name}"
        f ()
    
    let run targets =
        for t in targets do
            runTarget t
    
    type Shell with
        static member ExecSuccess (cmd: string, ?args: string, ?dir: string) =
            let res = Shell.Exec(cmd, ?args = args, ?dir = dir)
            if res <> 0 then failwith $"Shell execute was not successful: {res}" else ()

    type Args() =
        let singleArg = fsi.CommandLineArgs.[1..] |> Array.tryExactlyOne
        let mutable switches : string list = []
        member this.hasArg arg =
            switches <- arg :: switches
            singleArg |> Option.map (fun a -> a = arg) |> Option.defaultValue false
        member this.assertArgs() =
            match singleArg with
            | None ->
                let switches = switches |> String.concat "|"
                let msg = $"USAGE: dotnet fsi build.fsx [{switches}]"
                printfn "%s" msg
                Environment.Exit -1
            | _ -> ()

let args = Args()
let shallBuild = args.hasArg "build"
//let shallTest = args.hasArg "test"
let shallPublish = args.hasArg "publish"
let shallPack = args.hasArg "pack"
let shallFormat = args.hasArg "format"

do args.assertArgs()

let clean = "clean", fun () ->
    !! "../src/**/bin"
    ++ "../src/**/obj"
    ++ Paths.packPath
    |> Shell.cleanDirs 

let build = "build", fun () ->
    !! "../src/TypeProvider/Trulla/Trulla.fsproj"
    ++ "../src/SourceGenerator/Trulla.SourceGenerator.Core/Trulla.SourceGenerator.Core.fsproj"
    ++ "../src/SourceGenerator/Trulla.SourceGenerator/Trulla.SourceGenerator.csproj"
    |> Seq.iter (fun p ->
        let buildCommand = $"build {p} -c Release"
        Trace.trace $"SHELL> dotnet {buildCommand}"
        Shell.ExecSuccess ("dotnet", buildCommand))

//let test = "test", fun () ->
//    Shell.ExecSuccess ("dotnet", $"test {Paths.slnPath}")

let pack = "pack", fun () ->
    !! "../src/TypeProvider/Trulla/Trulla.fsproj"
    ++ "../src/SourceGenerator/Trulla.SourceGenerator.Core/Trulla.SourceGenerator.Core.fsproj"
    ++ "../src/SourceGenerator/Trulla.SourceGenerator/Trulla.SourceGenerator.csproj"
    |> Seq.iter (fun p ->
        let packCommand = sprintf "pack %s -o %s -c Release" p Paths.packPath
        Trace.trace $"SHELL> dotnet {packCommand}"
        Shell.ExecSuccess ("dotnet", packCommand)
    )

let format = "format", fun () ->
    Shell.ExecSuccess ("dotnet", $"fantomas ../src/TrullaProvider/Trulla/ ../src/TrullaProvider/Trulla.DesignTime/ --recurse")

// TODO: git tag + release
let publish = "publish", fun () ->
    let nugetApiKey = Environment.environVarOrFail Properties.nugetPushEnvVarName
    do !! $"{Paths.packPath}/*.nupkg" |> Seq.iter (fun p ->
        Shell.ExecSuccess ("dotnet", $"nuget push {p} -k {nugetApiKey} -s {Properties.nugetServer} --skip-duplicate")
    )

run [
    clean

    if shallBuild then
        build
    //if shallTest then
    //    test
    if shallPack then
        build
        pack
    if shallPublish then
        build
        pack
        publish
    if shallFormat then
        format
]

Trace.trace $"Finished script..."
