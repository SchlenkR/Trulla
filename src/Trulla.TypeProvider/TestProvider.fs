namespace Trulla.TypeProviderImplementation

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices

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

        let ctor = ProvidedConstructor(
            [],
            invokeCode = fun args -> <@@ () @@>
        )
        myType.AddMember(ctor)
        
        let ctor2 = ProvidedConstructor(
            [ProvidedParameter("InnerState", typeof<string>)],
            //invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
            invokeCode = fun args -> <@@ () @@>)
        myType.AddMember(ctor2)

        //let innerState = ProvidedProperty(
        //    "InnerState",
        //    typeof<string>,
        //    getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        //myType.AddMember(innerState)

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
