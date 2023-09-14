using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;

namespace TrullaGenerator.Tests;

public sealed class TrullaSourceGeneratorTests
{
    [Fact]
    public void Test()
    {
        var compilation = CSharpCompilation.Create("TestProject",
            new[] { CSharpSyntaxTree.ParseText("struct Test { }") },
            Basic.Reference.Assemblies.Net70.References.All,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var generator = new TrullaSourceGenerator();
        var sourceGenerator = generator.AsSourceGenerator();

        // trackIncrementalGeneratorSteps allows to report info about each step of the generator
        GeneratorDriver driver = CSharpGeneratorDriver.Create(
            generators: new ISourceGenerator[] { sourceGenerator },
            driverOptions: new GeneratorDriverOptions(default, trackIncrementalGeneratorSteps: true));

        // Run the generator
        driver = driver.RunGenerators(compilation);

        // Update the compilation and rerun the generator
        compilation = compilation.AddSyntaxTrees(CSharpSyntaxTree.ParseText("// dummy"));
        driver = driver.RunGenerators(compilation);

        // Assert the driver doesn't recompute the output
        var result = driver.GetRunResult().Results.Single();
        var allOutputs = result.TrackedOutputSteps.SelectMany(outputStep => outputStep.Value).SelectMany(output => output.Outputs);
        Assert.Collection(allOutputs, output => Assert.Equal(IncrementalStepRunReason.Cached, output.Reason));

        // Assert the driver use the cached result from AssemblyName and Syntax
        var assemblyNameOutputs = result.TrackedSteps["AssemblyName"].Single().Outputs;
        Assert.Collection(assemblyNameOutputs, output => Assert.Equal(IncrementalStepRunReason.Unchanged, output.Reason));

        var syntaxOutputs = result.TrackedSteps["Syntax"].Single().Outputs;
        Assert.Collection(syntaxOutputs, output => Assert.Equal(IncrementalStepRunReason.Unchanged, output.Reason));
    }
}