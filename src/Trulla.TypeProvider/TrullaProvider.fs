namespace Trulla.TypeProviderImplementation

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type TrullaProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)

    let ns = "Templates" // TODO
    let thisAssembly = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(thisAssembly, ns, "MyType", Some typeof<obj>)
        let staticParams = [ProvidedStaticParameter("value", typeof<string>)]
        do
            myType.DefineStaticParameters(
                parameters = staticParams,
                instantiationFunction = (fun typeName paramValues ->
                    match paramValues with
                    | [| :? string as value |] ->
                        let provider = ProvidedTypeDefinition(
                            thisAssembly,
                            ns,
                            typeName,
                            Some typeof<obj>
                        )

                        let myProp = ProvidedProperty(
                            "MyProperty", typeof<string>, isStatic = true,
                            getterCode = fun args -> <@@ "Hello world" @@>)
                        myType.AddMember(myProp)
    
                        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
                        myType.AddMember(ctor)
    
                        let ctor2 = ProvidedConstructor(
                            [ProvidedParameter("InnerState", typeof<string>)],
                            invokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
                        myType.AddMember(ctor2)
    
                        let innerState = ProvidedProperty(
                            "InnerState", typeof<string>,
                            getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
                        myType.AddMember(innerState)

                        provider.AddMember(myType)

                        provider

                    | _ -> failwith "That wasn't supported!"
                ))
            
        [myType]
    
    do
        this.AddNamespace(ns, createTypes())

[<assembly: TypeProviderAssembly()>]
do ()
