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

Console.WriteLine(model.Render());

Console.ReadLine();
