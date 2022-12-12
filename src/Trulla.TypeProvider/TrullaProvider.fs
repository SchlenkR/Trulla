namespace Trulla.TypeProviderImplementation

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type TrullaProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)
        ////assemblyReplacementMap=[("BasicGenerativeProvider.DesignTime", "BasicProvider")])

    let ns = "TrullaProvider"
    let asm = Assembly.GetExecutingAssembly()

    // TODO: check we contain a copy of runtime files, and are not referencing the runtime DLL
    ////do assert (typeof<BasicProvider.Helpers.SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (count:int) =
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor(
            [ProvidedParameter("InnerState", typeof<string>)],
            invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
        myType.AddMember(ctor2)

        for i in 1 .. count do 
            let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
            myType.AddMember(prop)
        asm.AddTypes [ myType ]

        myType

    let myParamType =
        let t = ProvidedTypeDefinition(asm, ns, "GenerativeProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters(
            [ProvidedStaticParameter("Count", typeof<int>)],
            fun typeName args -> createType typeName (unbox<int> args.[0]))
        t
    do
        this.AddNamespace(ns, [myParamType])

[<assembly: TypeProviderAssembly()>]
do ()
