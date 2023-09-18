namespace TODO;

using System;
using System.Collections.Generic;
using System.Linq;

public record Order
{
    public required string id { get; init; }
    public required bool isActive { get; init; }
}

public record Root
{
    public required List<Order> orders { get; init; }
    public required User user { get; init; }
}

public record User
{
    public required string name { get; init; }
}

public static class Rendering
{
    public static string Render(this Root model)
    {
        var __sb = new System.Text.StringBuilder();
        __sb.Append("\nHello ");
        __sb.Append(model.user.name);
        __sb.Append(", how are you?\n\nYour Orders\n===\n\n");
        foreach (var order in model.orders)
        {
            __sb.Append("\nID: ");
            __sb.Append(order.id);
            __sb.Append("\n(");
            if (order.isActive)
                __sb.Append("active");
            else
                __sb.Append("inactive");
            __sb.Append(")\n");
        }

        __sb.Append("\n            ");

        return __sb.ToString();
    }
}
