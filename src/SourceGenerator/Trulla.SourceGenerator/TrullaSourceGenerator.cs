using Microsoft.CodeAnalysis;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Trulla.Core;
using Trulla.Core.Solver;

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
                solveResult: Solver.solve(text.GetText(cancellationToken)!.ToString())
            ));

        initContext.RegisterSourceOutput(solvedTemplates, (spc, solvedTemplate) =>
        {
            // TODO: Better source gen error handling
            static string RenderErrors(IEnumerable<TrullaError> errors)
            {
                var errorsText = string.Join("\n",  errors.Select(it => it.ToString()));
                return $@"Error in template: {errorsText}";
            }

            static string RenderContent(SolveResult solveResult) =>
                Trulla.SourceGenerator.Renderer.renderTemplate(solveResult);

            var finalContent = solvedTemplate.solveResult.IsOk
                ? RenderContent(solvedTemplate.solveResult.ResultValue)
                : RenderErrors(solvedTemplate.solveResult.ErrorValue);

            spc.AddSource(
                $"TrullaTemplates.{solvedTemplate.name}",
                finalContent);
        });
    }
}
