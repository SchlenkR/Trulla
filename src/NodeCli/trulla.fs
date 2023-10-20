module Trulla.NodeCli

type Command = InputFile

let args = 
    Node.Api.``process``.argv
    |> Seq.skip 2
    |> String.concat "; "


    let renderErrors(errors: TrullaError seq) =
        let errorList = [ for error in errors do error.ToString() ]
        let singleLineErrors = 
            [ for error in errorList do 
                error
                    .Replace("\n", " - ")
                    .Replace("\r", "")
                    .Replace("\t", "    ")
            ]
        $"""Errors in Trulla template:

{String.Join("\n\n", errorList)};
"""

let renderTemplate template =
    let solution = Trulla.Core.Solver.solve(template)
