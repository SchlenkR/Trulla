namespace Trulla.Core.Ast

open Trulla.Core.Utils
open Trulla.Core.Parsing

type TVar =
    | Root
    | TVar of int

type private BindingContext = Map<string, TVar>

type TVal<'a> =
    { 
        range: Range
        tvar: TVar
        bindingContext: BindingContext
        value: 'a 
    }
    override this.ToString() = sprintf "(%A)%A" this.range this.value

type TExp =
    | Text of string
    | Hole of TVal<MemberExp>
    | For of ident: TVal<string> * exp: TVal<MemberExp> * body: TExp list
    | If of cond: TVal<MemberExp> * body: TExp list
    | Else of cond: TVal<MemberExp> * body: TExp list

and Body = BindingContext * TExp list

and MemberExp =
    | AccessExp of {| instanceExp: TVal<MemberExp>; memberName: string |}
    | IdentExp of string

type Typ =
    | Mono of string
    | Poly of name: string * typParam: Typ
    | Field of Field
    | Record of TVar
    | Var of TVar

//// TODO: After solving, a transition should happen from Type to FinalTyp
//type FinalTyp =
//    | FMono of string
//    | FPoly of name: string * typParam: FinalTyp
//    | FField of Field
//    | FRecord of TVar

and Field = 
    { 
        name: string
        typ: Typ
    }

type AstResult = Result<TExp list, TrullaError list>

[<RequireQualifiedAccess>]
type private Scope =
    | IfOrElseScope of cond: TVal<MemberExp>
    | Other

module TVal =
    let create range tvar bindingContext value =
        { range = range; tvar = tvar; bindingContext = bindingContext; value = value }

[<RequireQualifiedAccess>]
module Ast =
    // TODO: meaningful error messages + location
    // TODO: Don't throw; return TemplateError results
    let buildTree (tokens: PVal<Token> list) : AstResult =
        let newTVar =
            let mutable x = -1
            fun () ->
                x <- x + 1
                ////printfn $"TVAR {x} %s{name}"
                TVar x

        let buildMemberExp bindingContext pexp =
            let rec ofPExpZero (pexp: PVal<MemberToken>) =
                let newTVal value = TVal.create pexp.range (newTVar()) bindingContext value
                match pexp.value with
                | IdentToken ident ->
                    newTVal (IdentExp ident)
                | AccessToken accExp ->
                    let accExp = {| instanceExp = ofPExpZero accExp.instanceExp; memberName = accExp.memberName |}
                    newTVal (AccessExp accExp)
            ofPExpZero pexp

        let mutable currTokIdx = 0
        let mutable openScopeStack = []
        let mutable elseBlockOpen = false
        let rec toTree (bindingContext: BindingContext) =
            
            let mutable scopeClosed = false
            let mutable revTree = []
            let addToken x = revTree <- x :: revTree

            do while not scopeClosed && currTokIdx < tokens.Length do
                let token = tokens.[currTokIdx]
                currTokIdx <- currTokIdx + 1

                let raiseTrullaEx message =
                    { 
                        ranges = [token.range]
                        message = message
                    }
                    |> TrullaException
                    |> raise

                match token.value with
                | Token.Text x -> 
                    let x = Text x
                    do addToken x
                | Token.Hole x -> 
                    let x = Hole (buildMemberExp bindingContext x)
                    do addToken x
                | Token.For (ident, acc) ->
                    let accExp = buildMemberExp bindingContext acc
                    let tvarIdent = newTVar()
                    do openScopeStack <- Scope.Other :: openScopeStack
                    let x = 
                        For (
                            TVal.create ident.range tvarIdent bindingContext ident.value,
                            accExp,
                            toTree (Map.add ident.value tvarIdent bindingContext))
                    do addToken x
                | Token.If acc ->
                    let cond = buildMemberExp bindingContext acc
                    do openScopeStack <- (Scope.IfOrElseScope cond) :: openScopeStack
                    let x = 
                        If (
                            cond,
                            toTree bindingContext)
                    do addToken x
                | Token.Else ->
                    if elseBlockOpen then
                        do elseBlockOpen <- false
                        let matchingIfCond =
                            match openScopeStack with
                            | (Scope.IfOrElseScope cond) :: _ -> cond
                            | _ -> raiseTrullaEx "An else needs an if."
                        let x = 
                            Else (
                                matchingIfCond,
                                toTree bindingContext)
                        do addToken x
                    else
                        do currTokIdx <- currTokIdx - 1
                        do elseBlockOpen <- true
                        do scopeClosed <- true
                | Token.End ->
                    match openScopeStack with
                    | [] -> 
                        raiseTrullaEx "Closing a scope is not possible without having a scope open." 
                    | _::xs -> 
                        do openScopeStack <- xs
                        do scopeClosed <- true
            revTree |> List.rev

        try 
            let tree = toTree Map.empty
            if openScopeStack.Length > 0 then
                // TODO: Range.zero is wrong
                { ranges = [Range.zero]; message = "TODO: Unclosed scope detected." }
                |> List.singleton
                |> Error
            else
                Ok tree
        with TrullaException err ->
            Error [err]
