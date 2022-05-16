
#I "../src/Trulla.TypeProvider/bin/Debug/netstandard2.0"
#r "Trulla.TypeProvider.dll"

type T = TrullaProvider.GenerativeProvider<Count = 2>
T("xxx").Property2
