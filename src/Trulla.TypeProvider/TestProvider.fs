namespace Trulla.TypeProviderImplementation

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open UncheckedQuotations
open Microsoft.FSharp.Quotations

[<TypeProvider>]
type TrullaProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("Trulla.TypeProvider", "TrullaProvider")],
        addDefaultProbingLocation = true
    )

    let ns = "Trulla"

    //// check we contain a copy of runtime files, and are not referencing the runtime DLL
    //do assert (typeof<BasicProvider.Helpers.SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (count:int) =
        let tempAsm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(tempAsm, ns, typeName, Some typeof<obj>, isErased=false)

        let innerStateBackingField =
            ProvidedField("_innerState", typeof<string>)
        let innerStateProp =
            ProvidedProperty(
                "InnerState",
                typeof<string>,
                getterCode =
                    function
                    | [ this ] -> Expr.FieldGetUnchecked(this, innerStateBackingField)
                    | _ -> failwith "invalid property getter params"
            )
        myType.AddMember(innerStateBackingField)
        myType.AddMember(innerStateProp)
        
        let ctor = ProvidedConstructor(
            [],
            invokeCode = fun args -> <@@ () @@>
        )
        myType.AddMember(ctor)

        let ctor2 = 
            ProvidedConstructor(
                [ProvidedParameter("innerState", typeof<string>)],
                invokeCode =
                    function
                    | [this; innerStateArg] ->
                        Expr.Sequential(
                            Expr.FieldSetUnchecked(this, innerStateBackingField, innerStateArg),
                            <@@ () @@>)
                    | args -> failwith $"Invalid property setter params: {args}"
            )
        myType.AddMember(ctor2)

        for i in 1 .. count do 
            let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
            myType.AddMember(prop)

        tempAsm.AddTypes [ myType ]

        myType

    let asm = Assembly.GetExecutingAssembly()
    let myParamType = 
        let t = ProvidedTypeDefinition(asm, ns, "TrullaProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters(
            [ProvidedStaticParameter("Count", typeof<int>)],
            fun typeName args -> createType typeName (unbox<int> args.[0]))
        t
    do
        this.AddNamespace(ns, [myParamType])


[<assembly: TypeProviderAssembly()>]
do ()
