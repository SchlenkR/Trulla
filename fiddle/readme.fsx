#r "nuget: Trulla,1.2.1"

open Trulla

let [<Literal>] ForTmpl = """
{{for x in numbers}}{{x}}{{end}}
"""

type ForTmplType = Template<ForTmpl>

// Instanciate a typed model for the tempalte.
let templateModel = ForTmplType.Root(["a"; "b"; "c"])

// Render and print it:
printfn $"{ForTmplType.Render(templateModel)}"



let [<Literal>] ForTmpl2 = """
{{for x in numbers|;}}{{x}}{{end}}
"""

type ForTmplType2 = Template<ForTmpl2>

// Instanciate a typed model for the tempalte.
let templateModel = ForTmplType2.Root(["a"; "b"; "c"])

// Render and print it:
printfn $"{ForTmplType2.Render(templateModel)}"





let [<Literal>] IfTmpl = """
Order is {{if order.isActive}}active{{else}}closed{{end}}.
"""

type IfTmplType = Template<IfTmpl>

// Instanciate a typed model for the tempalte.
let templateModel = IfTmplType.Root(IfTmplType.order(true))

// Render and print it:
printfn $"{IfTmplType.Render(templateModel)}"



