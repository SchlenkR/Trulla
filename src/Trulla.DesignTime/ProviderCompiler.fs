module Trulla.DesignTime.Internal.ProviderCompiler

open Trulla
open Trulla.Core.Ast
open Trulla.Core.Inference
open Trulla.Core.Solver

open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open Microsoft.FSharp.Quotations

module Consts =
    let providerNamespaceName = "Trulla"
    let modelArgName = "model"

module private Expr =
    let allSequential exprs =
        exprs |> List.fold (fun a b -> Expr.Sequential(a, b)) <@@ () @@>

let createRecords (recordDefs: RecordDef list) =
    let toProvidedRecord (def: RecordDef) =
        ProvidedTypeDefinition(def.name, Some typeof<obj>, isErased = false)
    let finalizeProvidedRecord 
        (providedRecords: Map<TVar, ProvidedTypeDefinition>)
        (providedRecord: ProvidedTypeDefinition)
        (def: RecordDef) 
        =
        let fields =
            [ for field in def.fields do
                let fieldType =
                    let rec toDotnetType typ =
                        match typ with
                        | Mono KnownTypes.string -> typeof<string>
                        | Mono KnownTypes.bool -> typeof<bool>
                        | Poly (KnownTypes.sequence, pt) ->
                            typedefof<List<_>>.MakeGenericType([| toDotnetType pt |])
                        | Record tvar -> providedRecords.[tvar] :> System.Type
                        // TODO:
                        // See comments in Ast.fs / FinalTyp: This gap has to be eliminated.
                        // See test "Free vars and Issue 7 Var(n)"
                        | Var _ -> typeof<obj>
                        | _ -> failwithf "Unsupported reference for type '%A'." typ
                    toDotnetType field.typ
                let fieldName = "_" + field.name
                let provField = ProvidedField(fieldName, fieldType)
                let provProperty = 
                    ProvidedProperty(
                        field.name, 
                        fieldType,
                        getterCode = fun args -> Expr.FieldGet(args.[0], provField)
                    )

                do
                    providedRecord.AddMember(provField)
                    providedRecord.AddMember(provProperty)
                provField,fieldType,provProperty
            ]
        let ctor = 
            ProvidedConstructor(
                fields |> List.map (fun (provField,fieldType,provProperty) ->
                    ProvidedParameter(provProperty.Name, fieldType)),
                invokeCode =
                    function
                    | this :: args ->
                        List.zip args fields
                        |> List.map (fun (arg, (f,_,_)) -> Expr.FieldSetUnchecked(this, f, arg))
                        |> Expr.allSequential
                    | args -> failwithf "Invalid ctor params: %A" args
            )
        do
            providedRecord.AddMember(ctor)
        (
            def,
            providedRecord,
            fields |> List.map (fun (_,_,provProperty) -> provProperty)
        )

    let providedRecords =
        recordDefs
        |> List.map (fun recDef -> recDef, toProvidedRecord recDef)
    let providedRecordsMap = 
        providedRecords 
        |> List.map (fun (recDef, providedRec) -> recDef.id, providedRec)
        |> Map.ofList
    let finalizedRecords = 
        providedRecords
        |> List.map (fun (recDef, providedRec) -> finalizeProvidedRecord providedRecordsMap providedRec recDef)
    finalizedRecords

let createRenderMethod 
    (providedRootRecord: ProvidedTypeDefinition) 
    (template: string) 
    (modelArgName: string)
    =
    ProvidedMethod(
        "Render",
        [ProvidedParameter(modelArgName, providedRootRecord)],
        typeof<string>,
        isStatic = true,
        invokeCode = fun args ->
            let boxedRoot = Expr.Coerce(args.[0], typeof<obj>)
            <@@ Rendering.reflectionRender (%%boxedRoot) template @@>
    )

let createTypeDefForStringLiteral typeName (template: string) =
    let solveResult = Solver.solve template
    match solveResult with
    | Error errors -> failwithf "Template error: %A" errors
    | Ok solveResult ->
        let providedRecords = createRecords solveResult.records
        let providedRootRecord =
            providedRecords
            |> List.find (fun (r,_,_) ->
                // Wieder sowas: Es sollte klar sein, dass es genau einen Root-Recotd geben MUSS
                match r.id with Root -> true | _ -> false)
            |> fun (_,provRec,_) -> provRec
        let renderFunction =
            //let recordsAndFields =
            //    providedRecords
            //    |> List.map (fun (recDef,provRec,props) -> recDef.id, (provRec,props))
            //    |> Map.ofList
            //RenderCompiler.createRenderMethod providedRootRecord recordsAndFields solveResult.tree
            createRenderMethod providedRootRecord template Consts.modelArgName
        let asm = ProvidedAssembly()
        let modelType = 
            ProvidedTypeDefinition(
                asm,
                Consts.providerNamespaceName,
                typeName,
                Some typeof<obj>,
                isErased = false,
                hideObjectMethods = true
            )
        do 
            modelType.AddMembers (providedRecords |> List.map (fun (_,provRec,_) -> provRec))
            modelType.AddMembers [renderFunction]
            asm.AddTypes [modelType]
        modelType
