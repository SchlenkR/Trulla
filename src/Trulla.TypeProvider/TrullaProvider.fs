namespace Trulla.TypeProviderImplementation

open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type TrullaProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)
        ////assemblyReplacementMap=[("BasicGenerativeProvider.DesignTime", "BasicProvider")])

    let ns = "Templates" // TODO
    
    // TODO: check we contain a copy of runtime files, and are not referencing the runtime DLL
    ////let asm = Assembly.GetExecutingAssembly()
    ////do assert (typeof<BasicProvider.Helpers.SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)

    let asm = ProvidedAssembly()

    let types = Map.empty

    let createType typeName =
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor(
            [ProvidedParameter("Template", typeof<string>)],
            invokeCode = fun args -> <@@ (%%(args.[0]): string) :> obj @@>)
        myType.AddMember(ctor)

        //for i in 1 .. count do 
        //    let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
        //    myType.AddMember(prop)
        //asm.AddTypes [ myType ]

        myType

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
