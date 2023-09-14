using Microsoft.CodeAnalysis;

[Generator]
public sealed class TrullaSourceGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext initContext)
    {
        // define the execution pipeline here via a series of transformations:

        // find all additional files that end with .txt
        var textFiles = initContext.AdditionalTextsProvider
            .Where(static file => file.Path.EndsWith(".txt"));

        // read their contents and save their name
        var namesAndContents = textFiles.Select(
            (text, cancellationToken) => (
                name: Path.GetFileNameWithoutExtension(text.Path),
                content: text.GetText(cancellationToken)!.ToString()
            ));

        // generate a class that contains their values as const strings
        initContext.RegisterSourceOutput(namesAndContents, (spc, nameAndContent) =>
        {
            spc.AddSource(
                $"ConstStrings.{nameAndContent.name}", 
                $@"
    public static partial class ConstStrings
    {{
        public const string {nameAndContent.name} = ""{nameAndContent.content}"";
    }}"
                );
        });
    }
}
