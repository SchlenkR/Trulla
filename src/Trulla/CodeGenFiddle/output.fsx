
#if INTERACTIVE
#else
namespace TODO
#endif

[<AutoOpen>]
module Text =
    
    open System.Text
    
    type StringBuffer = StringBuilder -> unit
    
    type TextBuilder () =
        member inline _.Yield (txt: string) = fun (b: StringBuilder) -> 
            Printf.bprintf b "%s" txt
        member inline _.Yield (c: char) = fun (b: StringBuilder) -> 
            Printf.bprintf b "%c" c
        member inline _.Yield (strings: #seq<string>) = fun (b: StringBuilder) ->
            for s in strings do Printf.bprintf b "%s\n" s
        member inline _.Yield (i: int) = fun (b: StringBuilder) -> 
            Printf.bprintf b "%d" i
        member inline _.Yield (f: float) = fun (b: StringBuilder) -> 
            Printf.bprintf b "%f" f
        member inline _.YieldFrom ([<InlineIfLambda>] f: StringBuffer) =
            f
        member inline _.Combine ([<InlineIfLambda>] f,[<InlineIfLambda>] g) = fun (b: StringBuilder) ->
            f b; g b
        member inline _.Delay ([<InlineIfLambda>] f) = fun (b: StringBuilder) ->
            (f()) b
        member inline _.Zero () =
            ignore
        member inline _.For (xs: 'a seq, [<InlineIfLambda>] f: 'a -> StringBuffer) = fun (b: StringBuilder) ->
            use e = xs.GetEnumerator ()
            while e.MoveNext() do
                (f e.Current) b
        member inline _.While ([<InlineIfLambda>] p: unit -> bool, [<InlineIfLambda>] f: StringBuffer) = fun (b: StringBuilder) ->
            while p () do f b
        member inline _.Run ([<InlineIfLambda>] f: StringBuffer) =
            let b = StringBuilder()
            do f b
            b.ToString()
    
    let text = TextBuilder()
    
    let quot = "\""
    let plus = " + "
    let pareno = "("
    let parenc = ")"
    let br = "\n"
    
    let ind indent (txt: string) = text { String.replicate indent "    "; txt }
    let lni indent (txt: string) = ind indent (text { txt; br })
    let ln = lni 0
    let stri indent (txt: string) = ind indent (text { quot; txt; quot })
    let str (txt: string) = stri 0 txt
    let strlni indent (txt: string) = ln (stri indent txt)
    let strln (txt: string) = ln (str txt)


let x =
    text {
        "Numbers from 0 to 10:"
        for x in 0..10 do
            string x
        "\n"
        "Have a nice day!"
    }


let customer = {|
    name = "Best Plants"
    address = {|
        street = "Mulholland Drive"
        zipCode = "55512"
    |}
    orders = [
        {| id = 1; qty = 10; isDelivered = false |}
        {| id = 2; qty = 20; isDelivered = false |}
        {| id = 3; qty = 30; isDelivered = true |}
    ]
|}


text {
"Good day, "; customer.name ;"! here are your orders:
===
"
for order in customer.orders do
"
ID: "; order.id ;"
Quantity: ";order.qty; "
Status: "; if order.isDelivered then "You got it." else ">>> ON OUR WAY!"
"

Good night."
}


$"""
Good day, {customer.name}! here are your orders:
===
{for order in customer.orders do}

ID: {order.id}
Quantity: {order.qty}
Status: {if order.isDelivered then}You got it.{else}>>> ON OUR WAY!{end}
{done}

Good night."
"""




[<AutoOpen>]
module rec ModelTypes =

    type User = {
        name: string
    }

    type Order = {
        isActive: bool
        id: string
    }

    type Root = {
        orders: list<Order>
        user: User
    }


module Template =
    open ModelTypes

    let render (model: Root) =
        text {
            "
Hello "
            model.user.name
            ", how are you?

Your Orders
---
"
            for order in model.orders do
                text {
                    "ID: "
                    order.id
                    "
"
                    if order.isActive then
                        text {
                            "ORDER IS ACTIVE"
                            }
                    "
"
                    }
            "
"
            }

module Test =

    Template.render {
        user = { name = "Ronald" }
        orders = [
            {
                id = "Order_01"
                isActive = true
            }
            {
                id = "Order_02"
                isActive = false
            }
            {
                id = "Order_03"
                isActive = true
            }
        ]
    }
