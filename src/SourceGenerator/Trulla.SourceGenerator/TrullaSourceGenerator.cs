using Microsoft.CodeAnalysis;
using Trulla.Core;

[Generator]
public sealed class TrullaSourceGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext initContext)
    {
        var trullaTemplateFiles = initContext.AdditionalTextsProvider
            .Where(static file => file.Path.EndsWith(".txt"));

        var solvedTemplates = trullaTemplateFiles.Select(
            (text, cancellationToken) => (
                name: Path.GetFileNameWithoutExtension(text.Path),
                solution: Solver.solve(text.GetText(cancellationToken)!.ToString())
            ));

        initContext.RegisterSourceOutput(solvedTemplates, (spc, solvedTemplate) =>
        {
            // TODO: Better source gen error handling
            static string RenderErrors(IEnumerable<TrullaError> errors)
            {
                var errorsText = string.Join("\n",  errors.Select(it => it.ToString()));
                return $@"Error in template: {errorsText}";
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
