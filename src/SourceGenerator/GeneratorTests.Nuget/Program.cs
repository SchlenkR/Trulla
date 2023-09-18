// See https://aka.ms/new-console-template for more information
Console.WriteLine("Hello, World!");

var x = new[] { 1, 2, 3 };

foreach (var (item, index) in x.Select((__x, i) => (__x, i)))
{
    Console.WriteLine($"{item} {index}");
}

