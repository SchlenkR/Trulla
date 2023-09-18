using Microsoft.CodeAnalysis;
using Trulla.Core;

[Generator]
public sealed class TrullaSourceGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext initContext)
    {
        var trullaTemplateFiles = initContext.AdditionalTextsProvider
            .Where(static file => file.Path.EndsWith(".trulla"));

        var solvedTemplates = trullaTemplateFiles.Select(
            (text, cancellationToken) => (
                name: Path.GetFileNameWithoutExtension(text.Path),
                solution: Solver.solve(text.GetText(cancellationToken)!.ToString())
            ));

        initContext.RegisterSourceOutput(
            solvedTemplates,
            (spc, solvedTemplate) =>
            {
                // TODO: Better source gen error handling
                static string RenderErrors(IEnumerable<TrullaError> errors)
                {
                    var errorList = errors.Select(it => it.ToString()).ToList();
                    var singleLineErrors = errorList.Select(
                        it => it
                            .Replace("\n", " - ")
                            .Replace("\r", "")
                            .Replace("\t", "    "));
                    
                    return $@"""
#error Error in Trulla template {string.Join(";; ", singleLineErrors)}

/*

Errors in template:
------------------

{string.Join("\n\n", errorList)};

*/
                    """;
                }

                static string RenderContent(Trulla.Core.Solution solution) =>
                    Trulla.SourceGenerator.Renderer.renderTemplate(solution);

                var finalContent = solvedTemplate.solution.IsOk
                    ? RenderContent(solvedTemplate.solution.ResultValue)
                    : RenderErrors(solvedTemplate.solution.ErrorValue);

                spc.AddSource(
                    $"TrullaTemplates.{solvedTemplate.name}",
                    finalContent);
            });
    }
}
