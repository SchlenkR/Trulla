module Trulla.Typing

open Parsing

type TemplateError = { message: string; range: Range }

type Tree =
    | LeafNode of LeafToken
    | InternalNode of root: ScopeToken * children: Tree list
and LeafToken =
    | Text of string
    | Hole of PVal<AccessExp>
and ScopeToken =
    | For of ident: PVal<string> * exp: PVal<AccessExp>
    | If of PVal<AccessExp>

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

type TypeId = TypeId of string list

type Type =
    | Mono of TypeId
    | Poly of name:string * typParam:TypeId
    | Record of RecordDef
    | Any
and RecordDef = { id: TypeId; fields: FieldDef list }
and FieldDef = { name: string; typ: Type }

module KnownTypes =
    // TODO: reserve these keywords + parser tests
    let string = TypeId ["string"]
    let bool = TypeId ["bool"]
    let sequence elemTypId = "sequence", elemTypId

type Constraint =
    | IsOfType of Type
    | IsRecord
    | HasField of FieldDef

// TODO: DU
type Ident = string

type ExprConstraint = { typeId: TypeId; range: Range; constr: Constraint }

module private List =
    let rollOut elements =
        let buildRes path curr isLast = {| path = path; curr = curr; isLast = isLast |}
        let rec rollOut elements current =
            [ match elements with
              | [] -> ()
              | x :: (_::_ as remaining) ->
                  yield buildRes current x false
                  yield! rollOut remaining (current @ [x])
              | [x] ->
                  yield buildRes current x true
            ]
        rollOut elements []

// TODO: Don't allow shadowing
let collectConstraints (trees: Tree list) : ExprConstraint list * Map<Range, Type> =
    let constrainAccessExp (boundSymbols: Map<Ident, TypeId>) (pvalAccExp: PVal<_>) finalType =
        // TODO: Try revert lists and use "::" instead of " @ []"
        let isRooted,resolvedTypeId =
            let acc = pvalAccExp.value
            let isRooted,head =
                match boundSymbols |> Map.tryFind acc.ident with
                | None -> true, [acc.ident]
                | Some (TypeId tid) -> false, tid
            let res = head @ acc.propPath
            printfn $"RESOLVED (rooted={isRooted}): '{acc.ident}' -> {res}"
            isRooted,res
        [ for x in List.rollOut resolvedTypeId do
            let buildConstraint tid constr =
                { typeId = TypeId tid; range = pvalAccExp.range; constr = constr }
            
            match x.path with
            | _::_ as path ->
                yield buildConstraint path IsRecord
            | _ -> ()
            
            match isRooted, x.path, x.curr, x.isLast with
            | true, path, curr, true ->
                yield buildConstraint path (HasField { name = "a" + curr; typ = finalType })
            | true, [], curr, false ->
                yield buildConstraint [] (HasField { name = "b" + curr; typ = Mono (TypeId [curr]) })
            | _, (_::_ as path), curr, false ->
                yield buildConstraint path (HasField { name = "c" + curr; typ = Mono (TypeId (path @ [curr])) })
            | _ -> ()
        ]
    let newTypeId =
        let mutable x = -1
        fun (ident: string) ->
            x <- x + 1
            TypeId [ $"T{x}'{ident}" ]
    // TODO: is that good + do we need that? It's an optimization and the mutability seems unobsious here.
    let mutable rangesToTypes = Map.empty<Range, Type>
    let rec buildConstraints (trees: Tree list) (boundSymbols: Map<Ident, TypeId>) =
        [ for tree in trees do
            let addR2T range typ =
                do rangesToTypes <- rangesToTypes |> Map.add range typ
            match tree with
            | LeafNode (Text _) ->
                ()
            | LeafNode (Hole hole) ->
                let typ = Mono  KnownTypes.string
                yield! constrainAccessExp boundSymbols hole typ
                do addR2T hole.range typ
            | InternalNode (For (ident,source), children) ->
                let newTypeId = newTypeId ident.value
                let sourceTyp = Poly (KnownTypes.sequence newTypeId)
                yield! constrainAccessExp boundSymbols source sourceTyp
                do addR2T source.range sourceTyp

                let newBoundSymbols = boundSymbols |> Map.add ident.value newTypeId
                // TODO: TyVar, not MONO!
                do addR2T ident.range (Mono newTypeId)
                yield! buildConstraints children newBoundSymbols
            | InternalNode (If cond, children) ->
                let typ = Mono KnownTypes.bool
                yield! constrainAccessExp boundSymbols cond typ
                do addR2T cond.range typ
                yield! buildConstraints children boundSymbols
        ]
    let res = buildConstraints trees Map.empty
    res,rangesToTypes

type UnificationResult =
    { typeId: TypeId
      errors: TemplateError list
      resultingTyp: Type }

let unifyConstraints (constraints: ExprConstraint list) =
    constraints
    |> List.groupBy (fun x -> x.typeId)
    |> List.map (fun (typeId,constraints) ->
        ({ typeId = typeId; errors = []; resultingTyp = Any }, constraints)
        ||> List.fold (fun state expConstr ->
            let addRecordExpectedError () =
                // TODO: Message
                let err =
                    { message = $"TODO: Record expected; mono or sequence infered."
                      range = expConstr.range }
                { state with errors = err :: state.errors }
            match expConstr.constr with
            | IsOfType typ ->
                match typ,state.resultingTyp with
                | Any,typ
                | typ,Any -> { state with resultingTyp = typ }
                | a,b when a = b -> state
                | a,b ->
                    let err = // TODO: Message
                        { message = $"TODO: Should be '{a}', but is infered to be '{b}'."
                          range = expConstr.range }
                    { state with errors = err :: state.errors }
            | IsRecord ->
                match state.resultingTyp with
                | Any -> { state with resultingTyp = Record { id = typeId; fields = [] }}
                | Record _ -> state
                | _ -> addRecordExpectedError ()
            | HasField field ->
                match state.resultingTyp with
                | Any -> { state with resultingTyp = Record { id = typeId; fields = [field] }}
                | Record r -> { state with resultingTyp = Record { r with fields = field :: r.fields }}
                | _ -> addRecordExpectedError ()
        )
    )
