namespace FSharp.Text.TypedTemplateProvider.DesignTime

open System.Reflection

open Trulla
open Trulla.Core.Ast
open Trulla.Core.Inference

open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

module private Consts =
    let providerNamespaceName = "FSharp.Text.TypedTemplateProvider"

module private Expr =
    let allSequential exprs =
        exprs |> List.fold (fun a b -> Expr.Sequential(a, b)) <@@ () @@>

module private ModelCompiler =
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
                    let fieldName = "_" + field.name
                    let provField = ProvidedField(fieldName, fieldType)
                    let provProperty = ProvidedProperty(
                        field.name, 
                        fieldType,
                        getterCode = fun args -> Expr.FieldGet(args[0], provField)
                    )

                    do
                        providedRecord.AddMember(provField)
                        providedRecord.AddMember(provProperty)
                    provField,fieldType,provProperty
                ]
            let ctor = 
                ProvidedConstructor(
                    fields |> List.map (fun (provField,fieldType,_) ->
                        ProvidedParameter(provField.Name, fieldType)),
                    invokeCode =
                        function
                        | this :: args ->
                            List.zip args fields
                            |> List.map (fun (arg, (f,_,_)) -> Expr.FieldSetUnchecked(this, f, arg))
                            |> Expr.allSequential
                        | args -> failwith $"Invalid ctor params: {args}"
                )
            do
                providedRecord.AddMember(ctor)
            (
                def,
                providedRecord,
                fields |> List.map (fun (_,_,provProperty) -> provProperty)
            )

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

module private RenderCompiler =
    let createRenderMethod 
        (providedRootRecord: ProvidedTypeDefinition) 
        (template: string) 
        (modelArgName: string) 
        =
        ////let rec createRenderExprs append (tree: TExp list) (bindingContext: Map<string, Expr * Type>) =
        ////    let rec accessMember (exp: TVal<MemberExp>) =
        ////        match exp.value with
        ////        | IdentExp ident -> 
        ////            bindingContext[ident] |> fst
        ////        | AccessExp acc ->
        ////            let instanceAccessExp = accessMember acc.instanceExp
        ////            let prop = 
        ////                providedRecords[acc.instanceExp.tvar] 
        ////                |> snd
        ////                |> List.find (fun prop -> prop.Name = acc.memberName)
        ////            Expr.PropertyGet(instanceAccessExp, prop)

        ////    [ for texp in tree do
        ////        match texp with
        ////        | Text txt ->
        ////            yield Expr.Value(txt) |> append
        ////        | Hole hole ->
        ////            yield accessMember hole |> fst |> append
        ////        | For (ident,exp,body) ->
        ////            yield Expr.Value("<<<FOR EXPR>>") |> append
        ////            let identType =
        ////                // TODO: Is this ok?
        ////                match solution[exp.tvar] with 
        ////                | Poly (_, typ) -> typ
        ////                | _ -> failwith $"Poly type expected"
        ////            let inExp = accessMember exp
        ////            let bindingContext = bindingContext |> Map.add ident.value inExp
        ////            let mapping =
        ////                let tmp = createRenderExprs append body bindingContext |> Expr.allSequential
        ////                tmp
        ////                //Expr.Lambda(
        ////                //    Var(ident.value, inExpAndType),
        ////                //    Expr.Value(())
        ////                //)
        ////            ()
        ////            ////let itemsExp = fst inExpAndType

        ////            //yield <@@ Runtime.iter (%%mapping) (%%itemsExp) @@>
        ////            //yield
        ////            //    <@@
        ////            //        for x in (%%itemsExp) do
        ////            //            (%%mapping) x
        ////            //    @@>
        ////        | If (cond,body) ->
        ////            yield 
        ////                Expr.IfThenElse(
        ////                    accessMember cond |> fst |> append,
        ////                    createRenderExprs append body bindingContext |> Expr.allSequential,
        ////                    Expr.Value(())
        ////                )
        ////    ]

        ////let invokeCode = fun (args: Expr list) ->
        ////    let boxedRoot = Expr.Coerce(Expr.Coerce(args[0], typeof<obj>), providedRootRecord)

        ////    let sbVar = Var("sb", typeof<StringBuilder>)
        ////    let withStringBuilder body =
        ////        Expr.Let(
        ////            sbVar,
        ////            Expr.NewObject(typeof<StringBuilder>.GetConstructor([||]), []),
        ////            body)
        ////    let append value =
        ////        Expr.Call(Expr.Var(sbVar), typeof<StringBuilder>.GetMethod("Append", [| typeof<string> |]), [value])
        ////    let sbToString = 
        ////        Expr.Call(Expr.Var(sbVar), typeof<StringBuilder>.GetMethod("ToString", [||]), [])
        ////    // TODO: We use reflection too often; we have the info, but it gets lost
        ////    let rootBindingContext =
        ////        [ for p in providedRootRecord.GetProperties() do
        ////            let propertyGet = Expr.PropertyGet(Expr.Coerce(boxedRoot, providedRootRecord), p)
        ////            p.Name, (propertyGet, p.PropertyType)
        ////        ]
        ////        |> Map.ofList

        ////    [
        ////        yield! createRenderExprs append tree rootBindingContext
        ////        yield sbToString
        ////    ]
        ////    |> Expr.allSequential
        ////    |> withStringBuilder

        ProvidedMethod(
            "Render",
            [ProvidedParameter(modelArgName, providedRootRecord)],
            typeof<string>,
            isStatic = true,
            invokeCode = fun args ->
                let boxedRoot = Expr.Coerce(args[0], typeof<obj>)
                <@@ Rendering.reflectionRender (%%boxedRoot) template @@>
        )
    
[<TypeProvider>]
type TemplateProviderImplementation(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(
        config,
        assemblyReplacementMap = [("FSharp.Text.TypedTemplateProvider.DesignTime", "FSharp.Text.TypedTemplateProvider")],
        addDefaultProbingLocation = true
    )
    
    let providerName = "Template"
    // TODO: Allow for passing file name instead of string literal
    let templateParameterName = "TextTemplate"
    let modelArgName = "model"

    let asm = Assembly.GetExecutingAssembly()
    
    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<TpRuntime>.Assembly.GetName().Name = asm.GetName().Name)  

    let templateProviderForStringLiteral =
        let providerType = ProvidedTypeDefinition(
            asm, Consts.providerNamespaceName, providerName, Some typeof<obj>, isErased = false)
        do providerType.DefineStaticParameters(
            [ProvidedStaticParameter(templateParameterName, typeof<string>)],
            fun typeName args ->
                let template = unbox<string> args.[0]
                let solveResult = Solver.solve template
                match solveResult with
                | Error errors -> failwith $"Template error: {errors}"
                | Ok solveResult ->
                    let providedRecords = ModelCompiler.addRecords solveResult.records
                    let providedRootRecord =
                        providedRecords
                        |> List.find (fun (r,_,_) ->
                            // Wieder sowas: Es sollte klar sein, dass es genau einen Root-Recotd geben MUSS
                            match r.id with Root -> true | _ -> false)
                        |> fun (_,provRec,_) -> provRec
                    let renderFunction =
                        //let recordsAndFields =
                        //    providedRecords
                        //    |> List.map (fun (recDef,provRec,props) -> recDef.id, (provRec,props))
                        //    |> Map.ofList
                        //RenderCompiler.createRenderMethod providedRootRecord recordsAndFields solveResult.tree
                        RenderCompiler.createRenderMethod providedRootRecord template modelArgName
                    let asm = ProvidedAssembly()
                    let modelType = ProvidedTypeDefinition(
                        asm, Consts.providerNamespaceName, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)
                    do 
                        modelType.AddMembers (providedRecords |> List.map (fun (_,provRec,_) -> provRec))
                        modelType.AddMembers [renderFunction]
                        asm.AddTypes [modelType]
                    modelType
        )

        providerType
    do
        this.AddNamespace(Consts.providerNamespaceName, [templateProviderForStringLiteral])
