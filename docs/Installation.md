# Installation

Package can be installed with dotnet by following these steps:

* Add a source in your NuGet.config file
```
dotnet nuget add source "https://nuget.pkg.github.com/YelenaZvyagina/index.json"
```

* Authorize with your github token
```
paket config add-token "https://nuget.pkg.github.com/YelenaZvyagina/index.json" <token>
```

* Install the package
```
dotnet add PROJECT package BigIntCalculator --version <version>
```