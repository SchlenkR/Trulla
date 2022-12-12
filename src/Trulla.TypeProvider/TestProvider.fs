namespace Trulla.TypeProviderImplementation

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open UncheckedQuotations
open Microsoft.FSharp.Quotations

type TpType =
    | DotnetType of Type
    | TypeByName of string
type FieldDef =
    {
        name: string
        fieldType: TpType
    }
type RecordDef =
    {
        name: string
        fields: FieldDef list
    }

module TP =
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
                            | TypeByName s -> findRecordType s
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
type TrullaProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("Trulla.TypeProvider", "TrullaProvider")],
        addDefaultProbingLocation = true
    )

    //// check we contain a copy of runtime files, and are not referencing the runtime DLL
    //do assert (typeof<BasicProvider.Helpers.SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)  

    let asm = Assembly.GetExecutingAssembly()
    let ns = "Trulla"
    let tpEntry =
        let providerType = ProvidedTypeDefinition(asm, ns, "Template", Some typeof<obj>, isErased = false)
        providerType.DefineStaticParameters(
            [ProvidedStaticParameter("Count", typeof<int>)],
            fun typeName args -> 
                let modelDef =
                    [
                        {
                            name = "Record1"
                            fields = [
                                { name = "MyString1"; fieldType = DotnetType typeof<string> }
                                { name = "MyString2"; fieldType = DotnetType typeof<string> }
                            ]
                        }
                        {
                            name = "Record2"
                            fields = [
                                { name = "MyString1"; fieldType = DotnetType typeof<string> }
                                { name = "MyParent"; fieldType = TypeByName "Record1" }
                            ]
                        }
                    ]
                
                //createType typeName (unbox<int> args.[0])
                let asm = ProvidedAssembly()
                
                let records = TP.addRecords modelDef
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
        this.AddNamespace(ns, [tpEntry])


[<assembly: TypeProviderAssembly()>]
do ()
