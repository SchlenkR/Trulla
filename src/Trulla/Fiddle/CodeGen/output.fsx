#if INTERACTIVE
#else
namespace TODO
#endif

[<AutoOpen>]
module rec ModelTypes =

    type Order = {
        isActive: bool
        id: string
    }

    type Root = {
        orders: list<Order>
        user: User
    }

    type User = {
        name: string
    }


module Template =
    open System
    open ModelTypes

    let render (model: Root) =
        let __sb = System.Text.StringBuilder()
        ("
Hello " |> __sb.Append |> ignore)

        ("model.user.name" |> __sb.Append |> ignore)

        (", how are you?

Your Orders
---
" |> __sb.Append |> ignore)

        for order in model.orders do
            ("ID: " |> __sb.Append |> ignore)

            ("order.id" |> __sb.Append |> ignore)

            ("
" |> __sb.Append |> ignore)

            if order.isActive then
                ("ORDER IS ACTIVE" |> __sb.Append |> ignore)

            ("
" |> __sb.Append |> ignore)

        ("
" |> __sb.Append |> ignore)

