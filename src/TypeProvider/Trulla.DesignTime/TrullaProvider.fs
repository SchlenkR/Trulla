﻿namespace Trulla.DesignTime

open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open Trulla
open Trulla.DesignTime.Internal

module Consts =
    let providerName = "Template"
    let templateParameterName = "TextTemplate"

[<TypeProvider>]
type TemplateProviderImplementation(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(
        config,
        assemblyReplacementMap = 
            [
                "Trulla.DesignTime", "Trulla"
            ],
        addDefaultProbingLocation = true
    )
    
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<TpRuntime>.Assembly.GetName().Name = asm.GetName().Name)

    let templateProviderForStringLiteral =
        let providerType =
            ProvidedTypeDefinition(
                asm,
                ProviderCompiler.Consts.providerNamespaceName,
                Consts.providerName,
                Some typeof<obj>,
                isErased = false
            )
        do providerType.DefineStaticParameters(
            [ProvidedStaticParameter(Consts.templateParameterName, typeof<string>)],
            fun typeName args -> 
                let template = unbox<string> args.[0]
                ProviderCompiler.createTypeDefForStringLiteral typeName template
        )

        providerType

    do
        this.AddNamespace(ProviderCompiler.Consts.providerNamespaceName, [templateProviderForStringLiteral])
