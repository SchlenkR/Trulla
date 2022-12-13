namespace Trulla.TypeProviderImplementation

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Trulla.Internal
open Trulla.Internal.Inference

module internal ModelCompiler =
    let addRecords (recordDefs: RecordDef list) =
        let toProvidedRecord (def: RecordDef) =
            let recordName =
                match def.id with
                | Root -> "Root"
                | TVar _ -> 
                    // TODO: How we know that we have at least one?
                    // TODO: Pascal case names / general: name checks all over the place
                    def.potentialNames[0]
            ProvidedTypeDefinition(recordName, Some typeof<obj>, isErased = false)
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
                            | Record tvar -> providedRecords[tvar]
                            // TODO: See comments in ModelInference / FinalTyp: This gap has to be eliminated
                            //| Var _ -> "obj"
                            | _ -> failwith $"Unsupported reference for type '{typ}'."
                        toDotnetType field.typ
                    let provField = ProvidedField(field.name, fieldType)
                    do providedRecord.AddMember(provField)
                    provField,fieldType
                ]
            let ctor = 
                ProvidedConstructor(
                    fields |> List.map (fun (provField,fieldType) -> ProvidedParameter(provField.Name, fieldType)),
                    invokeCode =
                        function
                        | this :: args ->
                            List.zip args fields
                            |> List.map(fun (arg, (f,_)) -> Expr.FieldSetUnchecked(this, f, arg))
                            |> List.rev
                            |> List.fold (fun a b -> Expr.Sequential(a, b)) <@@ () @@>
                        | args -> failwith $"Invalid property setter params: {args}"
                )
            providedRecord.AddMember(ctor)
            def,providedRecord

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
        
[<TypeProvider>]
type TemplateProviderImplemtation (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("Trulla.TypeProvider", "TemplateProvider")],
        addDefaultProbingLocation = true
    )

    //// check we contain a copy of runtime files, and are not referencing the runtime DLL
    //do assert (typeof<BasicProvider.Helpers.SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)  

    let asm = Assembly.GetExecutingAssembly()
    let ns = "Trulla"
    
    let templateProviderForStringLiteral =
        let providerType = ProvidedTypeDefinition(asm, ns, "Template", Some typeof<obj>, isErased = false)
        do providerType.DefineStaticParameters(
            [ProvidedStaticParameter("Template", typeof<string>)],
            fun typeName args ->
                let solveResult =
                    let template = unbox<string> args.[0]
                    Parsing.parseTemplate template |> Inference.solve
                match solveResult with
                | Error errors -> failwith $"Template error: {errors}"
                | Ok solveResult ->
                    let addedRecords = ModelCompiler.addRecords solveResult.records
                    let rootRecord =
                        addedRecords
                        |> List.find (fun (r,_) ->
                            // Wieder sowas: Es sollte klar sein, dass es genau einen Root-Recotd geben MUSS
                            match r.id with Root -> true | _ -> false)
                        |> snd
                    let providedRecords = addedRecords |> List.map snd
                    let renderFunction =
                        ProvidedMethod(
                            "Render",
                            [ProvidedParameter("model", rootRecord)],
                            typeof<string>,
                            isStatic = true,
                            invokeCode = fun args -> <@@ "TODO" @@>)
                    
                    let asm = ProvidedAssembly()
                    let modelType = ProvidedTypeDefinition(
                        asm, ns, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)
                    do 
                        modelType.AddMembers providedRecords
                        modelType.AddMembers [renderFunction]
                        asm.AddTypes [modelType]
                    modelType
        )

        providerType
    do
        this.AddNamespace(ns, [templateProviderForStringLiteral])


[<assembly: TypeProviderAssembly()>]
do ()
