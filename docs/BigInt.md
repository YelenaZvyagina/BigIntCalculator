# BigInt

BigInt is a library used for a BigIntCalculator, it implements functions for long arithmetics.

## Type

An instance of BigInt consists of `isPos of Boolean` and `digits of <MyList<int>>`. `<MyList<int>> `consists of integers from 0 to 9

## Functions

* `ml1Greater (ml1:MyList<_>) (ml2:MyList<_>)` - checks, if the first number is greater than the second, while numbers are represented in lists
* `isEqual (ml1 : MyList<_>) (ml2 : MyList<_>)` - checks whether two numbers represented in lists are equal
* `toFirst (lst:MyList<_>)` - changes the structure of a number, represented in list, that the whole number is stored in "First"
* `removeZeros (ml : MyList<_>)` - removes zeros in the beginning of number, represented in list
* `reverseSign (bnt:bInt)` - changes a sign of a number
* `isOdd (bnt:bInt)` - checks whether the number is odd
* `zeroComplete (ml:MyList<_>, num:int, toEnd:Boolean` - adds proper amount of zeros to the number
* `becomeEqual (ml1:MyList<_>) (ml2:MyList<_>)` - equalizes lengths of two lists by adding zeros to the beginning of one of them
* `transferOdd (ml:MyList<_>)` - results in a list, which elements are digits from 0 to 9
* `bntEqual (bnt1:bInt) (bnt2:bInt)` - checks if two number are equal
* `sumMl (ml1:MyList<_>) (ml2:MyList<_>) ` - summs two numbers represented in lists
* `subMl (ml1:MyList<_>) (ml2:MyList<_>) ` - subtracts the second number from the first one, while numbers are represented in lists
* `sumBint (bnt1:bInt) (bnt2:bInt)` - summs two numbers
* `subBint (bnt1:bInt) (bnt2:bInt)` -subtracts the second number from the first one
* `multToNumMl (ml:MyList<_>) (num:int)` - multiplies the number represented in lists to an integer
* `multMl (ml1:MyList<_>) (ml2:MyList<_>) ` - multiplicates two numbers represented in lists
* `multBnt (bnt1:bInt) (bnt2:bInt)` - multiplicates two numbers
* `select (big:MyList<_>) (small:MyList<_>)` - finds the number that by multiplicating with the second number gives the closest product to the fisrt number, while numbers are represented in lists
* `divRemMl (ml1:MyList<_>) (ml2:MyList<_>)` - finds the quotient and remainder of dividing the first number by the second, while numbers are represented in lists
* `divBnt (bnt1:bInt) (bnt2:bInt)` - finds the quotient of dividing the first number by the second
* `remBnt (bnt1:bInt) (bnt2:bInt)` - finds the remainder of dividing the first number by the second
* `absBnt (bnt:bInt)` - returns an absolute value of a number 
* `toBinary (x:bInt)` - converts a number to it's binary representation
* `toPower (bnt:bInt) (num:bInt)` - returns a result of exponentiation 
* `strToBigint (s:String)` - converts string to a bInt
* `bntToString (bnt : bInt)` - converts bInt to a string

