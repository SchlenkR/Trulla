
#r "FSharp.Compiler.Service.dll"

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

// Create an interactive checker instance
let checker = FSharpChecker.Create()

/// Get untyped tree for a specified input
let getUntypedTree (file, input) = 
    // Get compiler options for the 'project' implied by a single script file
    let projOptions, diagnostics = 
        checker.GetProjectOptionsFromScript(file, input, assumeDotNetFramework=false)
        |> Async.RunSynchronously
    let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projOptions)
    // Run the first phase (untyped parsing) of the compiler
    let parseFileResults = 
        checker.ParseFile(file, input, parsingOptions) 
        |> Async.RunSynchronously
    parseFileResults.ParseTree

/// Walk over a pattern - this is for example used in 
/// let <pat> = <expr> or in the 'match' expression
let rec visitPattern e =
    match e with
    | SynPat.Wild(_) -> 
        printfn "  .. underscore pattern"
    | SynPat.Named(name, _, _, _) ->
        printfn "  .. named as '%s'" name.idText
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _, _) ->
        let names = String.concat "." [ for i in ident -> i.idText ]
        printfn "  .. identifier: %s" names
    | pat -> printfn "  .. other pattern: %A" pat

/// Walk over an expression - if expression contains two or three 
/// sub-expressions (two if the 'else' branch is missing), let expression
/// contains pattern and two sub-expressions
let rec visitExpression e = 
    match e with
    | SynExpr.IfThenElse(ifExpr=cond; thenExpr=trueBranch; elseExpr=falseBranchOpt) ->
        // Visit all sub-expressions
        printfn "Conditional:"
        visitExpression cond
        visitExpression trueBranch
        falseBranchOpt |> Option.iter visitExpression 
    | SynExpr.LetOrUse(_, _, bindings, body, _, _) ->
        // Visit bindings (there may be multiple 
        // for 'let .. = .. and .. = .. in ...'
        printfn "LetOrUse with the following bindings:"
        for binding in bindings do
            let (SynBinding(access, kind, isInline, isMutable, attrs, xmlDoc, data, headPat, retInfo, init, equalsRange, m, debugPoint)) = binding
            visitPattern headPat
            visitExpression init
        // Visit the body expression
        printfn "And the following body:"
        visitExpression body
    | expr -> printfn " - not supported expression: %A" expr

/// Walk over a list of declarations in a module. This is anything
/// that you can write as a top-level inside module (let bindings,
/// nested modules, type declarations etc.)
let visitDeclarations decls = 
    for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) ->
            // Let binding as a declaration is similar to let binding
            // as an expression (in visitExpression), but has no body
            for binding in bindings do
                let (SynBinding(
                    access, kind, isInline, isMutable, attrs, xmlDoc,
                    valData, pat, retInfo, body, equalsRange, m, sp)) = binding
                visitPattern pat 
                visitExpression body         
        | _ -> printfn " - not supported declaration: %A" declaration

/// Walk over all module or namespace declarations 
/// (basically 'module Foo =' or 'namespace Foo.Bar')
/// Note that there is one implicitly, even if the file
/// does not explicitly define it..
let visitModulesAndNamespaces modulesOrNss =
    for moduleOrNs in modulesOrNss do
        let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = moduleOrNs
        printfn "Namespace or module: %A" lid
        visitDeclarations decls



// Sample input as a multi-line string
let input =
    """
open System

module X =
    (*
    I am a Trulla tempalte.
    *)
    let foo() = ""
  """
let file = "sourceTest.fsx"

let tree = getUntypedTree (file, SourceText.ofString input)

// Extract implementation file details
match tree with
| ParsedInput.ImplFile(implFile) ->
    // Extract declarations and walk over them
    let (ParsedImplFileInput(fn, script, name, _, _, modules, _, _)) = implFile
    visitModulesAndNamespaces modules
| _ -> failwith "F# Interface file (*.fsi) not supported."


