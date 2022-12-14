namespace Trulla.TypeProviderImplementation

open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

module Expression =
    let finalExp =
        //// Working:
        //let varX = Var("x", typeof<string>)
        //Expr.Let(varX, Expr.Value("Hello"), Expr.Var(varX))

        //// Also working:
        //let varX = Var("x", typeof<string>)
        //let letExpr = Expr.Let(varX, Expr.Value("Hello"), Expr.Var(varX))
        //<@@
        //    (%%letExpr: string)
        //@@>
    
        // Not working (with or without Expr.Cast<string> / with type annotations or not: doesn't matter):
        let varExpr = Expr.Var(Var("x", typeof<string>))
        <@@
            let x = "World" in (%%varExpr: string)
        @@>


// -------------------------------
// TypeProvider bootstrapping
// -------------------------------

[<TypeProvider>]
type TemplateProviderImplemtation (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)
    let asm = Assembly.GetExecutingAssembly()
    let ns = "Trulla"
    let renderMethod = ProvidedMethod(
        "Render", [], typeof<string>, isStatic = true,
        invokeCode = fun args -> Expression.finalExp)
    let providedType = ProvidedTypeDefinition(
        asm, ns, "Template", Some typeof<obj>, isErased = true)
    do 
        providedType.AddMember(renderMethod)
        this.AddNamespace(ns, [providedType])

[<assembly: TypeProviderAssembly()>]
do ()
