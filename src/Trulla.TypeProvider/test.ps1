dotnet publish Trulla.TypeProvider.fsproj -f:netstandard2.0 /p:PublishProfile=FolderProfile.pubxml
dotnet run --project ./test/TrullaTpTest.fsproj
