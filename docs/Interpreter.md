# Interpreter

Interpreter can be used both by developers and users

## Developers

Firstly,  ` Main.parse <string>  ` creates an abstract syntax tree to interpretate code.

Then `Interpreter.run <ast>` returns three dictionaries. The first contains values of all variables in `AST.Expression` format,the second contains variables in `string` format, the third has only one key - `"print"` with string of result of interpretation.

Dot file with a syntax tree can be obtained by running `ToTree.ToDot <ast> <output file path>`.


### Another functions

* `processExpr (vDict:Dictionary<AST.VName,AST.Expression>) (expr:AST.Expression)` - return a result of a given expression in `BigInt` format
* `processStmt (vDict:Dictionary<AST.VName,AST.Expression>) (pDict:Dictionary<string,string>) (stmt:AST.Stmt)` - gets an expression from a statement and sets it's value to a dictionaries with variable as a key
* `calculate (ast:AST.Stmt list)` - assisting function to compute a result of code with a single statement
* `errorHighlight (line:int), (col:int), (msg:string), (lastToken:String)` - prints into console highlighted message with location of error in code. Located in module CliColors, that also includes declaration for colours, that are used.

### Code example:

	let program = 
    "let x = 12 
    print x"
	let ast = parse program
	let _, _, pDict = Interpreter.run ast
	printfn "%s" pDict.["print"]

After running the given code 12 will be printed into console.

## Users

There are four console commands in BigInt Calculator

* `--inputfile <file path>` - enter a file with code
* `--inputstring <string>` - enter a string with code
* `--compute` - return the result of interpretation of the code
* `--todot <file path>` - return dot code of syntax tree to the given file
	
You will get the result after running "BigIntCalculator.exe" from console with given commands