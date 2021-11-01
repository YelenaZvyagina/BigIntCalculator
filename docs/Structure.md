---
hide:
  - navigation
---
# Github repository structure

    BigIntCalculator
    ├── .config - dotnet tools
    ├── .github - GitHub Actions setup 
    ├── docs - documentation files in .md format
    ├── src - code of the tool
    │	├── BigInt - interpreter, MyList, BigInt libraries
    |	├── BigInt.Cli - command interface 
    ├── tests - tests
    |	├── BigInt.UnitTests - tests for BigInt library and interpreter
    ├── fsharplint.json - linter config
    ├── mkdocs.yml - MkDocs config
    └── BigInt.sln - solution file
    
