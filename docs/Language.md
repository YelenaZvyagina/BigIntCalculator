# Language guide

BigInteger calculator uses a simple programming language to process arithmetic expressions

Code consists of statements with variables' names and arithmetic expressions with them. Each arithmetic expression is defined as a variable, that can be used in other expressions or printed in console.


## Statements


There are two types of statements in the language.

* `print <vname>`

##### prints the result of computed expression related to a given variable

* `let <vname> = <expression>`

##### Declares a variable associated with the given expression. 

##### `<vname>` starts with a latin character and may include numbers and other letters

##### `<expression>` consists of numbers, other variables and arithmetic operators such as
##### ` +, -, *, /, %, ^, &, (, ), | ` 


## Expressions

* `Num of <bInt>` 
* `NVar of <VName>` 
* `Sum of <Expression * Expression>` 
* `Sub of <Expression * Expression>` 
* `Mul of <Expression * Expression>` 
* `Div of <Expression * Expression>` 
* `Rem of <Expression * Expression>` 
* `Pow of <Expression * Expression>` 
* `Bin of <Expression>` 
* `Abs of <Expression>` 

## Operators

* `+` - sums two expressions
* `-` - subtracts the second expression from the first expression, also acts as unary minus if immediatly followed by number
* `*` - multiplies two expressions
* `/` - divides the first expression by the second expression
* `%` - finds the remainder of division of two expressions
* `^` - raises the first expression to the power of second expression
* `&` - converts a number to it's binary representation
* `|` - acts as a brackets for returning absolute value of the expression
* `(`, `)` - parenthesis to specify operators' priority

## Code Example

        let x = 45 / 15 + | 10 - 22 | 

        let y = x * 6 

        print y

Given code should result in 90.