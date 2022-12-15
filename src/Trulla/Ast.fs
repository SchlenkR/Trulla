namespace Trulla.Internal.Ast

open Trulla.Internal.Utils
open Trulla.Internal.Parsing

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
    override this.ToString() = $"({this.range}){this.value}"

type TExp =
    | Text of string
    | Hole of TVal<MemberExp>
    | For of ident: TVal<string> * exp: TVal<MemberExp> * body: TExp list
    | If of cond: TVal<MemberExp> * body: TExp list

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
                | AccessToken accExp ->
                    let accExp = {| instanceExp = ofPExpZero accExp.instanceExp; memberName = accExp.memberName |}
                    newTVal (AccessExp accExp)
                | IdentToken ident ->
                    newTVal (IdentExp ident)
            ofPExpZero pexp

        let rec toTree (pointer: int) scopeDepth (bindingContext: BindingContext) =
            // TODO: Is all that really necessary - the mutable stuff?
            let mutable pointer = pointer
            let mutable scopeDepth = scopeDepth
            let mutable endTokenDetected = false
            let tree =
                [ while not endTokenDetected && pointer < tokens.Length do
                    let token = tokens[pointer]
                    pointer <- pointer + 1

                    let processBody bindingContext : TExp list =
                        let newPointer,children,newScopeDepth = toTree pointer (scopeDepth + 1) bindingContext
                        scopeDepth <- newScopeDepth
                        pointer <- newPointer
                        children

                    match token.value with
                    | Token.Text x -> Text x
                    | Token.Hole x -> Hole (buildMemberExp bindingContext x)
                    | Token.For (ident, acc) ->
                        let accExp = buildMemberExp bindingContext acc
                        let tvarIdent = newTVar()
                        For (
                            TVal.create ident.range tvarIdent bindingContext ident.value,
                            accExp,
                            processBody (Map.add ident.value tvarIdent bindingContext))
                    | Token.If acc ->
                        If (
                            buildMemberExp bindingContext acc,
                            processBody bindingContext)
                    | Token.End ->
                        match scopeDepth with
                        | 0 ->
                            { ranges = [token.range]
                              message = "Closing a scope is not possible without having a scope open." }
                            |> TrullaException
                            |> raise
                        | n ->
                            scopeDepth <- n-1
                            endTokenDetected <- true
                ]
            pointer,tree,scopeDepth
        try 
            let _,tree,scopeDepth = toTree 0 0 Map.empty
            if scopeDepth > 0 then
                // TODO: Range.zero is wrong
                { ranges = [Range.zero]; message = "TODO: Unclosed scope detected." }
                |> List.singleton
                |> Error
            else
                Ok tree
        with TrullaException err ->
            Error [err]
