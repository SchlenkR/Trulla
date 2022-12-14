namespace Trulla.TypeProviderImplementation

open System.Text
open System.Reflection
open Trulla.Internal
open Trulla.Internal.Inference
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

module private Expr =
    let allSequential exprs =
        exprs |> List.fold (fun a b -> Expr.Sequential(a, b)) <@@ () @@>

module internal ModelCompiler =
    let addRecords (recordDefs: RecordDef list) =
        let toProvidedRecord (def: RecordDef) =
            ProvidedTypeDefinition(def.name, Some typeof<obj>, isErased = false)
        let finalizeProvidedRecord 
            (providedRecords: Map<TVar, ProvidedTypeDefinition>)
            (providedRecord: ProvidedTypeDefinition)
            (def: RecordDef) 
            =
            let fields =
                [ for field in def.fields do
                    let fieldType =
                        let rec toDotnetType typ =
                            match typ with
                            | Mono KnownTypes.string -> typeof<string>
                            | Mono KnownTypes.bool -> typeof<bool>
                            | Poly (KnownTypes.sequence, pt) ->
                                typedefof<List<_>>.MakeGenericType([| toDotnetType pt |])
                            | Record tvar -> providedRecords[tvar]
                            // TODO: See comments in ModelInference / FinalTyp: This gap has to be eliminated
                            //| Var _ -> "obj"
                            | _ -> failwith $"Unsupported reference for type '{typ}'."
                        toDotnetType field.typ
                    let provField = ProvidedField(field.name, fieldType)
                    do providedRecord.AddMember(provField)
                    provField,fieldType
                ]
            let ctor = 
                ProvidedConstructor(
                    fields |> List.map (fun (provField,fieldType) -> ProvidedParameter(provField.Name, fieldType)),
                    invokeCode =
                        function
                        | this :: args ->
                            List.zip args fields
                            |> List.map (fun (arg, (f,_)) -> Expr.FieldSetUnchecked(this, f, arg))
                            |> Expr.allSequential
                        | args -> failwith $"Invalid property setter params: {args}"
                )
            providedRecord.AddMember(ctor)
            def,providedRecord

        let providedRecords =
            recordDefs
            |> List.map (fun recDef -> recDef, toProvidedRecord recDef)
        let providedRecordsMap = 
            providedRecords 
            |> List.map (fun (recDef, providedRec) -> recDef.id, providedRec)
            |> Map.ofList
        let finalizedRecords = 
            providedRecords
            |> List.map (fun (recDef, providedRec) -> finalizeProvidedRecord providedRecordsMap providedRec recDef)
        finalizedRecords

    let createRenderMethod rootRecord (tree: TExp list) =
        let memberAccessExp (exp: TVal<MemberExp>) =
            failwith "TODO"
            //match exp.value with
            //| IdentExp ident ->
            //    let isBound = exp.bindingContext |> Map.containsKey ident
            //    let rootPrefix = if isBound then "" else rootIdentifier + dotIntoMember
            //    rootPrefix + ident
            //| AccessExp acc -> (memberExpToIdent acc.instanceExp) + dotIntoMember + acc.memberName

        let rec createRenderExprs (tree: TExp list) =
            [ for texp in tree do
                match texp with
                | Text txt ->
                    yield <@ fun (append: string -> unit) -> append txt @>
                | Hole hole ->
                    ()
                    //yield <@ fun (append: string -> unit) -> append (memberExpToIdent hole) @>
                | For (ident,exp,body) ->
                    //lni indent $"for %s{ident.value} in {memberExpToIdent exp} do"
                    //render (indent + 1) body
                    ()
                | If (cond,body) ->
                    //lni indent $"if {memberExpToIdent cond} then"
                    //render (indent + 1) body
                    ()
            ]
        //let finalExp =
        //    createRenderExprs tree
        //    @ [ <@@ sb.ToString() @@> ]
        //    |> Expr.allSequential
        //let finalExp =
        //    let appendHello = <@ fun (append: string -> unit) -> append "Hello" @>
        //    <@@ 
        //        let sb = StringBuilder()
        //        let append (txt: string) = sb.Append(txt) |> ignore
        //        (%appendHello) append
        //        sb.ToString()
        //    @@>
        let finalExp =
            // Working:
            //let varX = Var("x", typeof<string>)
            //Expr.Let(varX, Expr.Value("Hello"), Expr.Var(varX))
            
            // Also working:
            //let varX = Var("x", typeof<string>)
            //let letExpr = Expr.Let(varX, Expr.Value("Hello"), Expr.Var(varX))
            //<@@
            //    (%%letExpr: string)
            //@@>

            // Not working (with or without cast / typed or untyped: doesn't matter):
            let varExpr = Expr.Var(Var("x", typeof<string>))
            <@@
                let x = "World" in (%%varExpr: string)
            @@>

        ProvidedMethod(
            "Render",
            [ProvidedParameter("model", rootRecord)],
            typeof<string>,
            isStatic = true,
            invokeCode = fun args -> finalExp)
        
[<TypeProvider>]
type TemplateProviderImplemtation (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("Trulla.TypeProvider", "TemplateProvider")],
        addDefaultProbingLocation = true
    )

    //// check we contain a copy of runtime files, and are not referencing the runtime DLL
    //do assert (typeof<BasicProvider.Helpers.SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)  

    let asm = Assembly.GetExecutingAssembly()
    let ns = "Trulla"
    
    let templateProviderForStringLiteral =
        let providerType = ProvidedTypeDefinition(asm, ns, "Template", Some typeof<obj>, isErased = false)
        do providerType.DefineStaticParameters(
            [ProvidedStaticParameter("Template", typeof<string>)],
            fun typeName args ->
                let solveResult =
                    let template = unbox<string> args.[0]
                    Parsing.parseTemplate template |> Inference.solve
                match solveResult with
                | Error errors -> failwith $"Template error: {errors}"
                | Ok solveResult ->
                    let providedRecords = ModelCompiler.addRecords solveResult.records
                    let rootRecord =
                        providedRecords
                        |> List.find (fun (r,_) ->
                            // Wieder sowas: Es sollte klar sein, dass es genau einen Root-Recotd geben MUSS
                            match r.id with Root -> true | _ -> false)
                        |> snd
                    let renderFunction = ModelCompiler.createRenderMethod rootRecord solveResult.tree
                    let asm = ProvidedAssembly()
                    let modelType = ProvidedTypeDefinition(
                        asm, ns, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)
                    do 
                        modelType.AddMembers (providedRecords |> List.map snd)
                        modelType.AddMembers [renderFunction]
                        asm.AddTypes [modelType]
                    modelType
        )

        providerType
    do
        this.AddNamespace(ns, [templateProviderForStringLiteral])


[<assembly: TypeProviderAssembly()>]
do ()
