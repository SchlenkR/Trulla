[<RequireQualifiedAccess>]
module Trulla.Rendering

open System.Text
open System.Collections
open System.Linq

open Trulla.Core.Utils
open Trulla.Core.Ast
open Trulla.Core.Solver
    
// - Why falling back to reflection?
//   1) Staying open for Fable
//   2) I want to get this thing done.
//      and using the IL emit approach
//      (commented code below) is quite complicated and things like
//      quotation splicing / list iteration don't seem to work the
//      way I think they should be (maybe I'm doing something wrong or
//      there are bugs / unimplemented things in TP-SDK quotation processing).
// - Also, we pass the template string and solve it _again_. This would basically
//   be unnecessary if the render method generation happened completely in the
//   generative TP.
let reflectionRender (model: obj) (template: string) =
    let tree = 
        let solveResult = Solver.solve template
        match solveResult with
        | Error errors -> failwithf "Template error: %A" errors
        | Ok solveResult -> solveResult.tree

    let sb = StringBuilder()
    let inline append (value: string) = sb.Append(value) |> ignore

    let rec render (bindingContext: Map<string, obj>) (tree: TExp list) =
        let rec getIdentBoundValue (exp: TVal<MemberExp>) : obj =
            match exp.value with
            | IdentExp ident -> 
                bindingContext |> Map.find ident "find ident in bindingContext"
            | AccessExp acc ->
                let instance = getIdentBoundValue acc.instanceExp
                let prop = instance.GetType().GetProperty(acc.memberName)
                prop.GetValue(instance)

        for texp in tree do
            match texp with
            | Text txt ->
                do append txt
            | Hole hole ->
                do getIdentBoundValue hole :?> string |> append
            | For (ident,exp,sep,body) ->
                let objList =
                    (getIdentBoundValue exp :?> IEnumerable).Cast<obj>()
                    |> Seq.indexed
                    |> Seq.toList
                let sep = sep.value |> Option.defaultValue ""
                for i,x in objList do
                    let bindingContext = bindingContext |> Map.add ident.value x
                    do render bindingContext body
                    if i < objList.Length - 1 then
                        do append sep
            | If (cond,body) ->
                let cond = getIdentBoundValue cond :?> bool
                if cond then
                    do render bindingContext body
            | Else (cond, body) ->
                let cond = getIdentBoundValue cond :?> bool
                if not cond then
                    do render bindingContext body

    let rootBindingContext =
        [ for p in model.GetType().GetProperties() do
            p.Name, p.GetValue(model)
        ]
        |> Map.ofList
    do render rootBindingContext tree
    sb.ToString()
