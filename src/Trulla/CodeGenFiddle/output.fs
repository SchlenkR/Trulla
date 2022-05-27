module rec TODO

type T0 = {
    orders: list<obj>
    isActive: bool
}

type T5 = {
    id: string
}

type TRoot = {
    x: T5
    customer: T0
}


let renderTemplate (model: TRoot) =
    let sb = System.Text.StringBuilder()

    sb.Append("
Hello
"
    ) |> ignore

    if model.customer.isActive then
        sb.Append("
ACTIVE
"\n        ) |> ignore

