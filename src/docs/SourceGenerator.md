# Trulla Templates for C#

C# Source Generator: [![NuGet Badge](http://img.shields.io/nuget/v/Trulla.svg?style=flat)](https://www.nuget.org/packages/Trulla)

Example and HowTo
---

Add a package reference to `Trulla.SourceGenerator`

In your project, add an item group for trulla template files:

```xml
    <ItemGroup>
    <AdditionalFiles Include="*.trulla" />
    </ItemGroup>
```

Add a trulla file (e.g. `TestTemplate.trulla`) with the following content:

```
Hello {{user.name}}, how are you?

Your Orders
===
{{for order in orders|---}}
ID: {{order.id}}
({{if order.isActive}}active{{else}}inactive{{end}})
{{end}}
```

To render the template, add the following code:

```csharp
using TestTemplate;

var model =
    new Root
    {
        user = new User { name = "Hans" },
        orders = new List<Order>
        {
            new() { id = "0", isActive = true },
            new() { id = "1", isActive = false },
            new() { id = "2", isActive = true }
        }
    };

var result = model.Render();

Console.WriteLine(result);
```

This will print:

```
Hello Hans, how are you?

Your Orders
===

ID: 0
(active)
---
ID: 1
(inactive)
---
ID: 2
(active)
```
