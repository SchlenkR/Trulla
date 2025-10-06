namespace Trulla.Core

open TheBlunt

open Trulla.Core
open Trulla.Core.Utils

type TVar =
    | Root
    | TVar of int

type BindingContext = Map<string, TVar>

type TVal<'a> =
    { 
        range: Range
        tvar: TVar
        bindingContext: BindingContext
        value: 'a 
    }
    override this.ToString() = sprintf "(%s)%s" (this.range.ToString()) (this.value.ToString())

type TExp =
    | Text of string
    | Hole of TVal<MemberExp>
    | For of ident: TVal<string> * exp: TVal<MemberExp> * sep: PVal<string option> * body: TExp list
    | If of cond: TVal<MemberExp> * body: TExp list
    | Else of cond: TVal<MemberExp> * body: TExp list

and Body = BindingContext * TExp list

and MemberExp =
    | AccessExp of {| instanceExp: TVal<MemberExp>; memberName: string |}
    | IdentExp of string

type Ast =
    {
        tree: TExp list
        tvarToMemberExp: Map<TVar, MemberExp>
    }

[<RequireQualifiedAccess>]
module Ast =

    [<RequireQualifiedAccess>]
    type private Scope =
        | IfOrElseScope of cond: TVal<MemberExp>
        | Other
    
    let createTVal range tvar bindingContext value =
        { range = range; tvar = tvar; bindingContext = bindingContext; value = value }
    
    // TODO: meaningful error messages + location
    // TODO: Don't throw; return TemplateError results
    let buildTree (tokens: PVal<Token> list) : Result<_,_> =
        // TODO: This is far from what I thing it could be in terms of understandable code
        
        let mutable tvarToMemberExp = Map.empty

        let newTVar =
            let mutable x = -1
            fun () ->
                x <- x + 1
                TVar x

        let buildMemberExp bindingContext pexp =
            let rec ofPExpZero (pexp: PVal<MemberToken>) =
                let newTVal tvar value = createTVal pexp.range tvar bindingContext value
                let newTValAndAdd exp =
                    let tvar = newTVar ()
                    do tvarToMemberExp <- tvarToMemberExp |> Map.add tvar exp
                    newTVal tvar exp
                match pexp.result with
                | IdentToken ident ->
                    newTValAndAdd (IdentExp ident)
                | AccessToken accExp ->
                    let accExp = {| instanceExp = ofPExpZero accExp.instanceExp; memberName = accExp.memberName |}
                    newTValAndAdd (AccessExp accExp)
            ofPExpZero pexp

        let mutable currTokIdx = 0
        let mutable openScopeStack = []
        let mutable elseBlockOpen = false
        let rec toTree (bindingContext: BindingContext) : Result<_,_> =
            let mutable scopeClosed = false
            let mutable revTree = []
            let mutable errors = []
            
            let addToken x = do revTree <- x :: revTree

            do while not scopeClosed && currTokIdx < tokens.Length do
                let token = tokens.[currTokIdx]
                currTokIdx <- currTokIdx + 1

                let addError message =
                    do errors <-
                        [
                            yield! errors
                            yield 
                                {
                                    range = token.range
                                    message = message
                                }
                        ]
                let addErrors newErrors =
                    do errors <- [ yield! errors; yield! newErrors ]

                match token.result with
                | Token.Text x -> 
                    let x = Text x
                    do addToken x
                | Token.Hole x -> 
                    let x = Hole (buildMemberExp bindingContext x)
                    do addToken x
                | Token.For (ident, acc, sep) ->
                    let accExp = buildMemberExp bindingContext acc
                    let tvarIdent = newTVar()
                    do openScopeStack <- Scope.Other :: openScopeStack
                    match toTree (Map.add ident.result tvarIdent bindingContext) with
                    | Ok subTree -> 
                        let x = 
                            (
                                createTVal ident.range tvarIdent bindingContext ident.result,
                                accExp,
                                sep,
                                subTree
                            )
                            |> For
                        do addToken x
                    | Error subErrors ->
                        do addErrors subErrors
                | Token.If acc ->
                    let cond = buildMemberExp bindingContext acc
                    do openScopeStack <- (Scope.IfOrElseScope cond) :: openScopeStack
                    match toTree bindingContext with
                    | Ok subTree -> do addToken (If (cond, subTree))
                    | Error subErrors -> do addErrors subErrors
                | Token.Else ->
                    if elseBlockOpen then
                        do elseBlockOpen <- false
                        match openScopeStack with
                        | (Scope.IfOrElseScope cond) :: _ ->
                            match toTree bindingContext with
                            | Ok subTree -> do addToken (Else (cond, subTree))
                            | Error subErrors -> do errors <- [ yield! errors; yield! subErrors ]
                        | _ ->
                            do addError "An else needs an if."
                    else
                        do currTokIdx <- currTokIdx - 1
                        do elseBlockOpen <- true
                        do scopeClosed <- true
                | Token.End ->
                    match openScopeStack with
                    | [] -> 
                        do addError "Closing a scope is not possible without having a scope open." 
                    | _::xs -> 
                        do openScopeStack <- xs
                        do scopeClosed <- true

            match errors with
            | [] -> revTree |> List.rev |> Ok
            | _ -> Error errors

        match toTree Map.empty with
        | Ok tree ->
            if openScopeStack.Length > 0 then
                // TODO: Range.zero is wrong
                { range = Range.zero; message = "TODO: Unclosed scope detected." }
                |> List.singleton
                |> Error
            else
                Ok { tree = tree; tvarToMemberExp = tvarToMemberExp }
        | Error errors -> Error errors
