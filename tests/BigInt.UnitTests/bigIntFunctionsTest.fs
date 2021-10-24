namespace BigIntCalculatorTests

open System
open System.Collections.Generic
open System.ComponentModel

module BigIntFunctionsTest = 

    open System.Numerics
    open BigIntCalculator
    open BigIntFunctions
    open Expecto.Flip
    open MyList
    open Expecto

    module BigintTests = 

        let rand = Random()
        let genRandomList l =
            List.init (rand.Next(1, l)) (fun _ -> rand.Next(9))

        let genRandomBigInteger l =
            let rList = (genRandomList l)
            let temp =  rList |> List.fold (fun (acc, dec) x ->  ( (BigInteger x) * dec + acc , dec * (BigInteger 10)  ) ) (BigInteger 0, BigInteger 1)  
            fst temp
            
        let bigIntegerToBigInt (num : BigInteger) =
            let strNum = num |> string
            let newX = if strNum.[0] = '-' then strNum.[1..] else strNum
            let list = sysListToMyList (List.map (fun elem -> int elem - int '0' ) (List.ofSeq newX) )
            bInt (list, (if num >= BigInteger 0 then true else false))

        let intToBinary x =
            let mutable r = if x = 0 then "0" else ""
            let mutable c = x
            while c > 0 do
                r <- string (c % 2) + r
                c <- c / 2
            int64 r
        
        [<Tests>]
        let testsheplers =
            testList "mylist functions and helpers" [
                testProperty "isEqual test" <| fun _ -> 
                    let x = bigIntegerToBigInt (genRandomBigInteger 20)
                    let y = x.digits
                    Expect.isTrue (isEqual x.digits y) "isEqual works incorrectly"
                    
                testProperty "ml1Greater test" <| fun _ ->
                    let x = bigIntegerToBigInt (genRandomBigInteger 20)
                    let z = x
                    let x1 = concat (First 1) x.digits
                    let z1 = concat (First 5) z.digits
                    Expect.isTrue (ml1Greater z1 x1) "ml1Greater with diff fst char works incorrectly"
                    let x2 = concat x.digits (First 2)
                    let z2 = concat z.digits (First 8)
                    Expect.isTrue (ml1Greater z2 x2) "ml1Greater with diff last char works incorrectly"
                    
                testProperty "strToBigint test" <| fun _ -> 
                    let x = genRandomBigInteger 20
                    let s = string x
                    let a = strToBigint s
                    let b = bigIntegerToBigInt x
                    Expect.isTrue (bntEqual a b) "str to bigint works incorrectly"
                    
                testProperty "removeZeros test" <| fun _ -> 
                    let zeros = 20
                    let zerList = sysListToMyList (List.init zeros (fun i -> i*0))
                    let x = (bigIntegerToBigInt (genRandomBigInteger 20) ).digits
                    let withzer = concat zerList x
                    Expect.isTrue (isEqual x (removeZeros withzer)) "removezeros works incorrectly"
                    
                testProperty "transferodd test" <| fun _ -> 
                    let x = genRandomBigInteger 8
                    let x2 = int x
                    let x1 = bigIntegerToBigInt x
                    let y1 = reverse (transferOdd (First x2) )
                    Expect.isTrue (isEqual y1 (x1.digits)) "transferodd works incorrectly"
                    
                testProperty "becomeEqual test" <| fun _ -> 
                    let x = (bigIntegerToBigInt (genRandomBigInteger 20) )
                    let y = (bigIntegerToBigInt (genRandomBigInteger 20) )
                    let res = (becomeEqual x.digits y.digits )
                    Expect.isTrue ( (length x.digits) = (length (fst res) ) || (length y.digits) = (length (fst res) ) )  "becomeequal works incorrectly"
            ]
            

     
        [<Tests>]
        let arFuncsTests =
            testList "test for arithmetical functions" [
                testProperty "sum test" <| fun _ ->
                   let x = genRandomBigInteger 20
                   let z = genRandomBigInteger 20
                   let s3 = (bigIntegerToBigInt (x + z) )
                   let s4 = (sumBint (bigIntegerToBigInt x) (bigIntegerToBigInt z))
                   Expect.isTrue (bntEqual s3 s4) "summ works incorrectly"
                  
                testProperty "sub test" <| fun _ -> 
                    let x = genRandomBigInteger 20
                    let y = genRandomBigInteger 20
                    let s1 = (bigIntegerToBigInt (x - y))
                    let s2 = (subBint (bigIntegerToBigInt x) (bigIntegerToBigInt y))
                    Expect.isTrue (bntEqual s1 s2) "subtraction works incorrectly"

                testProperty "mul test" <| fun _ ->
                    let x = genRandomBigInteger 10
                    let z = genRandomBigInteger 10
                    let s3 = bigIntegerToBigInt (x * z)
                    let s4 = multBnt (bigIntegerToBigInt x) (bigIntegerToBigInt z)
                    Expect.isTrue (bntEqual s3 s4) "multiplication works incorrectly"

                testProperty "div test" <| fun _ ->
                    let x = genRandomBigInteger 10
                    let y = genRandomBigInteger 10
                    if y = BigInteger 0
                    then
                        let y1 = y + x
                        let s1 = bigIntegerToBigInt (x / y1)
                        let s2 = divBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y1)
                        Expect.isTrue (bntEqual s1 s2) "division works incorrectly"
                    else
                        let s1 = bigIntegerToBigInt (x / y)
                        let s2 = divBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y)
                        Expect.isTrue (bntEqual s1 s2) "division works incorrectly"

                testProperty "remainder test" <| fun _ ->
                    let x = genRandomBigInteger 10
                    let y = genRandomBigInteger 10
                    if y = BigInteger 0
                    then
                        let y1 = y + x
                        let s1 = bigIntegerToBigInt (x % y1)
                        let s2 = remBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y1)
                        Expect.isTrue (bntEqual s1 s2) "finding remainder works incorrectly"
                    else 
                        let s1 = bigIntegerToBigInt (x % y)
                        let s2 = remBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y)
                        Expect.isTrue (bntEqual s1 s2) "finding remainder works incorrectly"   

                testProperty "pow test" <| fun _ ->
                    let x = genRandomBigInteger 5
                    let y = rand.Next(5)
                    let s = BigInteger.Pow(x, y) 
                    let s1 = toPower (bigIntegerToBigInt x) (bigIntegerToBigInt (BigInteger y)) 
                    Expect.isTrue (bntEqual (bigIntegerToBigInt s) s1) "pow works incorrectly"

                testProperty "toBinary test" <| fun _ ->
                    let x = rand.Next(10000) 
                    let s = BigInteger (intToBinary x)
                    let x1 = bigIntegerToBigInt (BigInteger x)
                    let sb = toBinary x1
                    Expect.isTrue (bntEqual (bigIntegerToBigInt s) sb) "toBinary works incorrectly"

                testProperty "Unary minus test" <| fun _ ->
                    let x = genRandomBigInteger 20
                    let minusX = x * (BigInteger -1)
                    let expAns = reverseSign (bigIntegerToBigInt x)
                    Expect.isTrue (bntEqual (bigIntegerToBigInt minusX) expAns) "Reversesign works incorrectly"
            ]
            
        [<Tests>]
        let helpersOnInt64Tests =
            testList "helpers for int64" [
                
                testProperty "isEqual test" <| fun _ ->
                    let x = int64 ( (rand.Next()) * Int32.MaxValue )
                    let y = x
                    let x1 = strToBigint (string x)
                    let y1 = strToBigint (string y)
                    Expect.isTrue (isEqual x1.digits y1.digits) "isEqual works incorrectly"
                    
                testProperty "ml1Greater test" <| fun _ ->
                    let x = int64 ( (rand.Next()) * Int32.MaxValue )
                    let y = int64 (rand.Next(1000))
                    let x1 = strToBigint (string x)
                    let y1 = strToBigint (string y)
                    Expect.isTrue (ml1Greater x1.digits y1.digits) "ml1Greater works incorrectly"
                    
                testProperty "transferodd test" <| fun _ ->
                    let x = (rand.Next(10)) 
                    let x1 = strToBigint (string x)
                    let y = reverse (transferOdd x1.digits )
                    Expect.isTrue (isEqual y (First x) ) "transferodd works incorrectly"
                    
                testProperty "becomeEqual test" <| fun _ ->
                    let x = int64 ( (rand.Next()) * Int32.MaxValue )
                    let y = int64 (rand.Next(1000))
                    let x1 = strToBigint (string x)
                    let y1 = strToBigint (string y)
                    let comp = length x1.digits
                    let res = (becomeEqual x1.digits y1.digits )
                    Expect.isTrue ( length (snd res) = comp )  "becomeequal works incorrectly"
            ]
            
        
            
        [<Tests>]
        let arithmOnInt64Tests = 
            testList "arithmetics for int64" [
                
                testProperty "Unary minus test" <| fun _ ->
                    let x = int64 ( (rand.Next()) * Int32.MaxValue )
                    let x1 = strToBigint (string x)
                    let xun = x *  int64 (-1)
                    let x1un = reverseSign x1
                    Expect.isTrue (bntEqual x1un (strToBigint (string xun)) ) "unary minus works incorrectly"
                    
                testProperty "sum test" <| fun _ ->
                   let x = int64 ( (rand.Next()) * Int32.MaxValue )
                   let y = int64 ( (rand.Next()) * Int32.MaxValue )
                   let x1 = strToBigint (string x)
                   let y1 = strToBigint (string y)
                   let sumBint = sumBint x1 y1
                   let sumNums = x + y
                   let comp = strToBigint (string sumNums)
                   Expect.isTrue (bntEqual sumBint comp) "sum works incorrectly"
                  
                testProperty "sub test" <| fun _ ->
                   let x = int64 ( (rand.Next()) * Int32.MaxValue )
                   let y = int64 ( (rand.Next()) * Int32.MaxValue )
                   let x1 = strToBigint (string x)
                   let y1 = strToBigint (string y)
                   let subBint = subBint x1 y1
                   let subNums = x - y
                   let comp = strToBigint (string subNums)
                   Expect.isTrue (bntEqual subBint comp) "subtraction works incorrectly"
                    
                testProperty "mul test" <| fun _ ->
                    let x = int64 (rand.Next(10000))
                    let y = int64 (rand.Next(10000))
                    let x1 = strToBigint (string x)
                    let y1 = strToBigint (string y)
                    let mulBint = multBnt x1 y1
                    let mulNums = x * y
                    let comp = strToBigint (string mulNums)
                    Expect.isTrue (bntEqual mulBint comp) "multiplication works incorrectly"
                    
                testProperty "div test" <| fun _ ->
                    let x = int64 (rand.Next(10000))
                    let y = int64 (rand.Next(10000) + 1) 
                    let x1 = strToBigint (string x)
                    let y1 = strToBigint (string y)
                    let divBint = divBnt x1 y1
                    let divNums = x / y
                    let comp = strToBigint (string divNums)
                    Expect.isTrue (bntEqual divBint comp) "division works incorrectly"
                    
                testProperty "remainder test" <| fun _ ->
                    let x = int64 (rand.Next(10000))
                    let y = int64 (rand.Next(10000))
                    let x1 = strToBigint (string x)
                    let y1 = strToBigint (string y)
                    let remBint = remBnt x1 y1
                    let remNums = x % y
                    let comp = strToBigint (string remNums)
                    Expect.isTrue (bntEqual remBint comp) "remainder works incorrectly"
            ]