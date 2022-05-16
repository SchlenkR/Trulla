namespace TrullaGen

open Microsoft.CodeAnalysis

[<Generator>]
type TrullaGen() =
    interface ISourceGenerator with
        member this.Initialize(context: GeneratorInitializationContext ) =
            ()

        member this.Execute(context: GeneratorExecutionContext) =
            let source = """ 
// Auto-generated code
using System;
namespace TrullaTemplate;

public static class TestClass
{
    public static void Say() => "Hello";
}
"""
            let typeName = "Hurz"
            do context.AddSource($"{typeName}.g.cs", source)
