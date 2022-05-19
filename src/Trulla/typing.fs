module Trulla.Typing

open Parsing

type Tree =
    | LeafNode of LeafToken
    | InternalNode of root: ScopeToken * children: Tree list
and LeafToken =
    | Text of string
    | Hole of PVal<Exp>
and ScopeToken =
    | For of ident: PVal<string> * exp: PVal<Exp>
    | If of PVal<Exp>

// TODO: meaningful error messages + location
// TODO: Don't throw; return TemplateError results
let buildTree (tokens: ParseResult) : Tree list =
    let mutable scopeStack = []
    let rec toTree (pointer: int) =
        let mutable pointer = pointer
        let nodes = 
            let mutable endTokenDetected = false
            [ while not endTokenDetected && pointer < tokens.Length do
                let token = tokens[pointer]
                pointer <- pointer + 1
                let descentWithNewScope scopeToken =
                    scopeStack <- scopeToken :: scopeStack
                    let newPointer,children = toTree pointer
                    let res = InternalNode (scopeToken, children)
                    pointer <- newPointer
                    res
                match token with
                | ParserToken.Text x -> yield LeafNode (Text x)
                | ParserToken.Hole x -> yield LeafNode (Hole x)
                | ParserToken.For (ident, acc) -> yield descentWithNewScope (For (ident, acc))
                | ParserToken.If acc -> yield descentWithNewScope (If acc)
                | ParserToken.End ->
                    match scopeStack with
                    | [] -> failwith "TODO: Closing an unopened scope"
                    | _ :: xs -> scopeStack <- xs
            ]
        pointer,nodes
    let tree = snd (toTree 0)
    if scopeStack.Length > 0
        then failwith "TODO: Unclosed scope detected."
        else tree

type TVar =
    | TVar of int
    | Root

type Type =
    | Mono of string
    | Poly of name:string * typParam:Type
    | Var of TVar // TODO: why VAR is in here?
    | RecordRef of RecordId
and RecordId = RecordId of string
and Field = string * Type

type Constraint =
    | IsOfType of Type
    | HasField of Field

type BindingContext = Map<string, TVar>
type Problem = Problem of Constraint * Constraint

module KnownTypes =
    // TODO: reserve these keywords + parser tests
    let string = "string"
    let bool = "bool"
    let sequence elemTypId = "sequence", elemTypId

// TODO: Prevent shadowing
let collectConstraints (trees: Tree list) =
    let newTVar =
        let mutable x = -1
        fun () ->
            x <- x + 1
            TVar x
    
    let rec constrainExp (bindingContext: BindingContext) exp =
        match exp with
        | AccessExp exp ->
            let tvarInstance,instanceProblems = constrainExp bindingContext exp.instanceExp
            let tvarExp = newTVar()
            let problems = [
                yield! instanceProblems
                ////Problem (tvarInstance, IsRecordDefinition)
                Problem (tvarInstance, HasField (exp.memberName, Var tvarExp))
            ]
            IsOfType (Var tvarExp),problems
        | IdentExp ident ->
            let tvarIdent = bindingContext |> Map.tryFind ident
            match tvarIdent with
            | Some tvarIdent ->
                IsOfType (Var tvarIdent),[]
            | None ->
                let tvarIdent = newTVar()
                let problems = [
                    Problem (IsOfType (Var Root), HasField (ident, Var tvarIdent))
                    // tvarRoot is also a record; but we can omit this
                ]
                IsOfType (Var tvarIdent),problems
    
    let rec buildConstraints (trees: Tree list) (bindingContext: BindingContext) =
        [ for tree in trees do
            match tree with
            | LeafNode (Text _) ->
                ()
            | LeafNode (Hole hole) ->
                let tvarHole,holeProblems = constrainExp bindingContext hole.value
                yield! holeProblems
                yield Problem (tvarHole, IsOfType (Mono KnownTypes.string))
            | InternalNode (For (ident,source), children) ->
                let tvarIdent = newTVar()
                let bindingContext = bindingContext |> Map.add ident.value tvarIdent
                let tvarSource,sourceProblems = constrainExp bindingContext source.value
                yield! sourceProblems
                yield Problem (tvarSource, IsOfType (Poly (KnownTypes.sequence (Var tvarIdent))))
                // --->
                yield! buildConstraints children bindingContext
            | InternalNode (If cond, children) ->
                let tvarCond,condProblems = constrainExp bindingContext cond.value
                yield! condProblems
                yield Problem (tvarCond, IsOfType (Mono KnownTypes.bool))
                // --->
                yield! buildConstraints children bindingContext
        ]
    
    // TODO: return also tvarRoot
    buildConstraints trees Map.empty

type FinalType =
    | Type of Type
    | Record of Record
and Record = { name: string; fields: Field list }

// TODO: Ranges wieder überall reinmachen
////type TemplateError = { message: string; range: Range }
type UnificationResult =
    { tvar: TVar
      errors: string list
      resultingTyp: FinalType }

// TODO: This is currently not used
type SubstResult = OpenOrUnknown | Determined

let solveProblems (problems: Problem list) =
    let rec substTyp tvarToReplace by typ =
        match typ with
        | Poly (name, typ) ->
            let res,t = substTyp tvarToReplace by typ
            res, Poly (name, t)
        | Var tvar when tvar = tvarToReplace -> Determined, by
        | Var _ -> OpenOrUnknown, typ
        | Mono _ | RecordRef _ -> Determined, typ

    let substConstraint tvarToReplace by (constr: Constraint) =
        match constr with
        | IsOfType typ ->
            let res,t = substTyp tvarToReplace by typ
            IsOfType t //res, IsOfType t
        | HasField (name,typ) ->
            let res,t = substTyp tvarToReplace by typ
            HasField (name, t) //res, HasField (name, t)

    let substituteProblems tvarToReplace by (problems: Problem list) =
        problems 
        |> List.map (fun (Problem (cl,cr)) ->
            let cl = substConstraint tvarToReplace by cl
            let cr = substConstraint tvarToReplace by cr
            Problem (cl, cr)
        )

    let rec solveProblems (problems: Problem list) =
        match problems with
        | [] -> []
        | Problem (cleft, cright) :: ps ->
            match cleft,cright with
            | IsOfType (Var tvar), HasField _
            | HasField _, IsOfType (Var tvar) ->
                let recordRef =
                    // TODO: better record naming / prevent collisions
                    let recordName = $"""TYP{match tvar with Root -> "ROOT" | TVar var -> string var}"""
                    RecordRef (RecordId recordName)
                substituteProblems tvar recordRef problems
            | IsOfType (Var tvar), IsOfType t
            | IsOfType t, IsOfType (Var tvar) ->
                substituteProblems tvar t ps
            | IsOfType (RecordRef _), HasField _
            | HasField _, IsOfType (RecordRef _) ->
                problems
            | _, _ ->
                failwith "TODO: Can't unify. Don't throw"
    
    let rec doSolve problems =
        let newProblems = solveProblems problems
        if set newProblems <> set problems 
            then doSolve newProblems
            else problems
    
    doSolve problems
