type Order = {
    readonly id : string
    readonly isActive : boolean
}

type Root = {
    readonly orders : Array<Order>
    readonly user : User
}

type User = {
    readonly name : string
}

export function render (model: Root) {
    let __s = '';
    __s += `Hello `;
    __s += model.user.name;
    __s += `, how are you?

Your Orders
===

`;
    let i_1 = 0;
    for (const order of model.orders) {
        __s += `
ID: `;
        __s += order.id;
        __s += `
(`;
        if (order.isActive) {
            __s += `active`;
        }
        else {
            __s += `inactive`;
        }
        __s += `)
`;
        if (i_1 < model.orders.length - 1) {
            __s += `---`;
        }
    }
    __s += ``;
    __s += `
`;

    return __s;
}
