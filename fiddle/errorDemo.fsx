#r "nuget: Trulla"

open Trulla

let [<Literal>] TmplString = """
{{for order in orders|---}}
    {{order.id}}
{{end}}
"""

type Tmpl = Template<TmplString>
