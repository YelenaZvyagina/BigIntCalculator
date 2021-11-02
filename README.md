# BigInt Calculator

Big Integer Calculator is a tool for computing arithmetic expressions with long integers. Contains libraries for long arithmetics and non-empty lists and an interpreter for a simple programming language.

## Builds

||Github Actions|
|------|:------:|
|**Build Status**|[![GitHub Actions](https://github.com/YelenaZvyagina/bigIntCalculator/workflows/Build/badge.svg?branch=master)](https://github.com/YelenaZvyagina/bigIntCalculator/actions?query=branch%3Amain) |
|**Build History**|[![Build History](https://buildstats.info/github/chart/YelenaZvyagina/bigIntCalculator)](https://github.com/YelenaZvyagina/bigIntCalculator/actions?query=branch%3Amain) |


## Getting started

Package can be installed with dotnet by following these steps:

* Add a source in your NuGet.config file
#
     dotnet nuget add source "https://nuget.pkg.github.com/YelenaZvyagina/index.json"

* Authorize with your github token
#
    paket config add-token "https://nuget.pkg.github.com/YelenaZvyagina/index.json" <token>

* Install the package
# 
    dotnet add PROJECT package BigIntCalculator --version <version>


## Usage

BigInt Calculator contains a console application with interpreter for a simple language processing arithmetic expressions and a library for long arithmetic on non-empty lists.

Code of the language consists of statements with variables' names and arithmetic expressions with them. Each arithmetic expression is defined as a variable, that can be used in other expressions or printed in console.

### Supported statements

    let vname = expression // Declares a variable associated with the given expression.
    print vname // prints the result of computed expression related to a given variable

### Example of code

    let x = 98 / 2
    print x
 

## Documentation

Overview of the tool and how to use it can be found at [docs](https://yelenazvyagina.github.io/bigIntCalculator/).

## Directory structure

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
