namespace Trulla

open System.Text
open System.Collections
open System.Linq

open Trulla.Core.Ast
    
// Why falling back to reflection?
// 1) Staying open for Fable
// 2) I want to get this thing done.
//    and using the IL emit approach
//    (commented code below) is quite complicated and things like
//    quotation splicing / list iteration don't seem to work the
//    way I think they should be (maybe I'm doing something wrong or
//    there are bugs / unimplemented things in TP-SDK quotation processing).
module Rendering =
    let reflectionRender (model: obj) (tree: TExp list) =
        let sb = StringBuilder()
        let inline append (value: string) = sb.Append(value) |> ignore

        let rec render (bindingContext: Map<string, obj>) (tree: TExp list) =
            let rec getIdentBoundValue (exp: TVal<MemberExp>) : obj =
                match exp.value with
                | IdentExp ident -> 
                    bindingContext[ident]
                | AccessExp acc ->
                    let instance = getIdentBoundValue acc.instanceExp
                    let prop = instance.GetType().GetProperty(acc.memberName)
                    prop.GetValue(instance)

            for texp in tree do
                match texp with
                | Text txt ->
                    append txt
                | Hole hole ->
                    getIdentBoundValue hole :?> string |> append
                | For (ident,exp,body) ->
                    let objSeq = (getIdentBoundValue exp :?> IEnumerable).Cast<obj>()
                    for x in objSeq do
                        let bindingContext = bindingContext |> Map.add ident.value x
                        render bindingContext body
                | If (cond,body) ->
                    let cond = getIdentBoundValue cond :?> bool
                    if cond then
                        render bindingContext body

        let rootBindingContext =
            [ for p in model.GetType().GetProperties() do
                p.Name, p.GetValue(model)
            ]
            |> Map.ofList
        do render rootBindingContext tree
        sb.ToString()
