namespace Trulla.TypeProviderImplementation

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Trulla.Internal

type TpType =
    | DotnetType of Type
    | DotnetTypeWithGenericRecordArgument of Type * string
    | RecordRef of string

type FieldDef =
    {
        name: string
        fieldType: TpType
    }

type RecordDef =
    {
        isRoot: bool
        name: string
        fields: FieldDef list
    }

//module SolveResult =
//    let toTpModel (solveResult: SolveResult) =
//        let modelDef =
//            [
//                for recName,fields in solveResult.records |> Map.toList do
//                    let fields =
//                        [ for (fname,ftype) in fields do
//                            { 
//                                name = fname
//                                fieldType =
//                                    match ftype with
//                                    | 
//                            }
//                        ]
//            ]

module internal TypeProviderHelper =
    let addRecords (recordDefs: RecordDef list) =
        let addRecord (def: RecordDef) =
            ProvidedTypeDefinition(def.name, Some typeof<obj>, isErased = false)
        let finalizeRecord (addedRecords: ProvidedTypeDefinition list) (def: RecordDef) =
            let findRecordType name = addedRecords |> List.find (fun x -> x.Name = name)
            let recordType = findRecordType def.name
            let fields =
                [
                    for field in def.fields do
                        let fieldType =
                            match field.fieldType with
                            | DotnetType t -> t
                            | RecordRef s -> findRecordType s
                        let provField = ProvidedField(field.name, fieldType)
                        do recordType.AddMember(provField)
                        provField,fieldType
                ]
            let ctor = 
                ProvidedConstructor(
                    [
                        for provField,fieldType in fields do
                            ProvidedParameter(provField.Name, fieldType)
                    ],
                    invokeCode =
                        function
                        | this :: args ->
                            List.zip args fields
                            |> List.map(fun (arg, (f,_)) -> Expr.FieldSetUnchecked(this, f, arg))
                            |> List.rev
                            |> List.fold (fun a b -> Expr.Sequential(a, b)) <@@ () @@>
                        | args -> failwith $"Invalid property setter params: {args}"
                )
            recordType.AddMember(ctor)
            recordType

        let addedRecords = recordDefs |> List.map addRecord
        let finalizedRecords = recordDefs |> List.map (finalizeRecord addedRecords)
        
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
        providerType.DefineStaticParameters(
            [ProvidedStaticParameter("Template", typeof<string>)],
            fun typeName args ->
                let solveResult =
                    let template = unbox<string> args.[0]
                    Parsing.parseTemplate template |> Inference.solve
                match solveResult with
                | Error errors -> failwith $"Template error: {errors}"
                | Ok solveResult->
                    //let modelDef =
                    //    [
                    //        for recName,fields in solveResult.records |> Map.toList do
                    //            let fields =
                    //                [ for field in fields do
                    //                    { name = field; fieldType = DotnetType typeof<string> }
                    //                ]
                    //    ]
                        
                    //    |> Map.map (fun recName fields ->
                    //        { name = recName; fields = fields |> List.map (fun field ->
                    //            ) }

                    let modelDef =
                        [
                            {
                                name = "Root"
                                isRoot = true
                                fields = [
                                    { name = "MyString1"; fieldType = DotnetType typeof<string> }
                                    { name = "MyParent"; fieldType = RecordRef "MyType" }
                                ]
                            }
                            {
                                name = "MyType"
                                isRoot = false
                                fields = [
                                    { name = "MyString1"; fieldType = DotnetType typeof<string> }
                                    { name = "MyString2"; fieldType = DotnetType typeof<string> }
                                ]
                            }
                        ]
                
                    let asm = ProvidedAssembly()
                
                    let records = TypeProviderHelper.addRecords modelDef
                    let renderFunction =
                        ProvidedMethod(
                            "Render", 
                            [ProvidedParameter("model", records[0])],
                            typeof<string>,
                            isStatic = true,
                            invokeCode = fun args -> <@@ "TODO" @@>)
                    let modelType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)
                    do 
                        modelType.AddMembers records
                        modelType.AddMembers [renderFunction]
                        asm.AddTypes [modelType]
                    modelType
        )

        providerType
    do
        this.AddNamespace(ns, [templateProviderForStringLiteral])


[<assembly: TypeProviderAssembly()>]
do ()
