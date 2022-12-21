
dotnet publish ./Trulla/Trulla.fsproj -f:netstandard2.0 -c Debug /p:PublishDir=../TrullaProvider.Tests/.tpPublish

dotnet run --project ./TrullaProvider.Tests/TrullaProvider.Tests.fsproj
