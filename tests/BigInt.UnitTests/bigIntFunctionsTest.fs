namespace BigIntCalculatorTests

open System

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
            BigInt (list, (num >= BigInteger 0))

        let intToBinary x =
            let mutable r = if x = 0 then "0" else ""
            let mutable c = x
            while c > 0 do
                r <- string (c % 2) + r
                c <- c / 2
            int64 r
        
        [<Tests>]
        let testsHeplers =
            testList "mylist functions and helpers" [
                testProperty "isEqual test" <| fun n ->
                    let x = bigIntegerToBigInt (genRandomBigInteger (abs n + 1) )
                    let y = x.digits
                    Expect.isTrue (isEqual x.digits y) "isEqual works incorrectly"
                    
                testProperty "ml1Greater test" <| fun n ->
                    let x = bigIntegerToBigInt (genRandomBigInteger (abs n + 1) )
                    let z = x
                    let x1 = concat (First 1) x.digits
                    let z1 = concat (First 5) z.digits
                    Expect.isTrue (ml1Greater z1 x1) "ml1Greater with diff fst char works incorrectly"
                    let x2 = concat x.digits (First 2)
                    let z2 = concat z.digits (First 8)
                    Expect.isTrue (ml1Greater z2 x2) "ml1Greater with diff last char works incorrectly"
                    
                testProperty "strToBigint test" <| fun n -> 
                    let x = genRandomBigInteger (abs n + 1)
                    let s = string x
                    let a = strToBigint s
                    let b = bigIntegerToBigInt x
                    Expect.isTrue (bntEqual a b) "str to bigint works incorrectly"
                    
                testProperty "removeZeros test" <| fun n -> 
                    let zeros = abs n + 1
                    let zerList = sysListToMyList (List.init zeros (fun i -> i*0))
                    let x = (bigIntegerToBigInt (genRandomBigInteger (abs n + 1) ) ).digits
                    let withzer = concat zerList x
                    Expect.isTrue (isEqual x (removeZeros withzer)) "removeZeros works incorrectly"
                    
                testProperty "transferOdd test" <| fun n ->
                    let x = genRandomBigInteger (abs n % 10 + 1) 
                    let x2 = int x
                    let x1 = bigIntegerToBigInt x
                    let y1 = reverse (transferOdd (First x2) )
                    Expect.isTrue (isEqual y1 (x1.digits)) "transferOdd works incorrectly"
                    
                testProperty "becomeEqual test" <| fun n -> 
                    let x = (bigIntegerToBigInt (genRandomBigInteger (abs n + 1) ) )
                    let y = (bigIntegerToBigInt (genRandomBigInteger (abs n + 1)) )
                    let res = (becomeEqual x.digits y.digits )
                    Expect.isTrue ( (length x.digits) = (length (fst res) ) || (length y.digits) = (length (fst res) ) )  "becomeEqual works incorrectly"
            ]
            

     
        [<Tests>]
        let arFuncsTests =
            testList "test for arithmetical functions" [
                testProperty "sum test" <| fun n ->
                   let x = genRandomBigInteger (abs n + 1)
                   let z = genRandomBigInteger (abs n + 1)
                   let s3 = (bigIntegerToBigInt (x + z) )
                   let s4 = (sumBint (bigIntegerToBigInt x) (bigIntegerToBigInt z))
                   Expect.isTrue (bntEqual s3 s4) "summ works incorrectly"
                  
                testProperty "sub test" <| fun n -> 
                    let x = genRandomBigInteger (abs n + 1)
                    let y = genRandomBigInteger (abs n + 1)
                    let s1 = (bigIntegerToBigInt (x - y))
                    let s2 = (subBint (bigIntegerToBigInt x) (bigIntegerToBigInt y))
                    Expect.isTrue (bntEqual s1 s2) "subtraction works incorrectly"

                testProperty "mul test" <| fun n ->
                    let x = genRandomBigInteger (abs n % 10 + 1) 
                    let z = genRandomBigInteger (abs n % 10 + 1) 
                    let s3 = bigIntegerToBigInt (x * z)
                    let s4 = multBnt (bigIntegerToBigInt x) (bigIntegerToBigInt z)
                    Expect.isTrue (bntEqual s3 s4) "multiplication works incorrectly"

                testProperty "div test" <| fun n ->
                    let x = genRandomBigInteger (abs n % 10 + 1) 
                    let y = genRandomBigInteger (abs n % 10 + 1) 
                    if y = BigInteger 0
                    then
                        let y1 = abs (y + x) + BigInteger 1
                        let s1 = bigIntegerToBigInt (x / y1)
                        let s2 = divBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y1)
                        Expect.isTrue (bntEqual s1 s2) "division works incorrectly"
                    else
                        let s1 = bigIntegerToBigInt (x / y)
                        let s2 = divBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y)
                        Expect.isTrue (bntEqual s1 s2) "division works incorrectly"

                testProperty "remainder test" <| fun n ->
                    let x = genRandomBigInteger (abs n % 10 + 1) 
                    let y = genRandomBigInteger (abs n % 10 + 1) 
                    if y = BigInteger 0
                    then
                        let y1 = abs (y + x) + BigInteger 1
                        let s1 = bigIntegerToBigInt (x % y1)
                        let s2 = remBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y1)
                        Expect.isTrue (bntEqual s1 s2) "rem works incorrectly"
                    else
                        let s1 = bigIntegerToBigInt (x % y)
                        let s2 = remBnt (bigIntegerToBigInt x) (bigIntegerToBigInt y)
                        Expect.isTrue (bntEqual s1 s2) "rem works incorrectly"  

                testProperty "pow test" <| fun n ->
                    let x = genRandomBigInteger (abs n % 5 + 1) 
                    let y = rand.Next(5)
                    let s = BigInteger.Pow(x, y) 
                    let s1 = toPower (bigIntegerToBigInt x) (bigIntegerToBigInt (BigInteger y)) 
                    Expect.isTrue (bntEqual (bigIntegerToBigInt s) s1) "pow works incorrectly"

                testProperty "toBinary test" <| fun n ->
                    let s = BigInteger (intToBinary (abs n + 1))
                    let x1 = bigIntegerToBigInt (BigInteger (abs n + 1))
                    let sb = toBinary x1
                    Expect.isTrue (bntEqual (bigIntegerToBigInt s) sb) "toBinary works incorrectly"

                testProperty "Unary minus test" <| fun n ->
                    let x = genRandomBigInteger (abs n + 1)
                    let minusX = x * (BigInteger -1)
                    let expAns = reverseSign (bigIntegerToBigInt x)
                    Expect.isTrue (bntEqual (bigIntegerToBigInt minusX) expAns) "reverseSign works incorrectly"
            ]
            
        [<Tests>]
        let helpersOnInt64Tests =
            testList "helpers tests on int64" [
                
                testProperty "isEqual test" <| fun (x : int64) ->
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
                    
                testProperty "transferOdd test" <| fun _ ->
                    let x = (rand.Next(10)) 
                    let x1 = strToBigint (string x)
                    let y = reverse (transferOdd x1.digits )
                    Expect.isTrue (isEqual y (First x) ) "transfeOodd works incorrectly"
                    
                testProperty "becomeEqual test" <| fun _ ->
                    let x = int64 ( (rand.Next()) * Int32.MaxValue )
                    let y = int64 (rand.Next(1000))
                    let x1 = strToBigint (string x)
                    let y1 = strToBigint (string y)
                    let comp = length x1.digits
                    let res = (becomeEqual x1.digits y1.digits )
                    Expect.isTrue ( length (snd res) = comp )  "becomeEqual works incorrectly"
            ]
            
        
            
        [<Tests>]
        let arithmOnInt64Tests = 
            testList "arithmetics for int64" [
                
                testProperty "Unary minus test" <| fun (x : int64) ->
                    let x1 = strToBigint (string x)
                    let xun = x *  int64 (-1)
                    let x1un = reverseSign x1
                    Expect.isTrue (bntEqual x1un (strToBigint (string xun)) ) "unary minus works incorrectly"
                    
                testProperty "sum test" <| fun (x : int64, y : int64) -> 
                   let x1 = strToBigint (string x)
                   let y1 = strToBigint (string y)
                   let sumBint = sumBint x1 y1
                   let sumNums = x + y
                   let comp = strToBigint (string sumNums)
                   Expect.isTrue (bntEqual sumBint comp) "sum works incorrectly"
                  
                testProperty "sub test" <| fun (x : int64, y : int64) ->
                   let x1 = strToBigint (string x)
                   let y1 = strToBigint (string y)
                   let subBint = subBint x1 y1
                   let subNums = x - y
                   let comp = strToBigint (string subNums)
                   Expect.isTrue (bntEqual subBint comp) "subtraction works incorrectly"
                    
                testProperty "mul test" <| fun (x, y) -> 
                    let x1 = strToBigint (string (x % 10000) )
                    let y1 = strToBigint (string (y % 10000) )
                    let mulBint = multBnt x1 y1
                    let mulNums = int64 (x * y)
                    let comp = strToBigint (string mulNums)
                    Expect.isTrue (bntEqual mulBint comp) "multiplication works incorrectly"
                    
                testProperty "div test" <| fun (x : int64, y : int64) -> 
                    let x1 = strToBigint (string x)
                    let y2 = if y = int64 0 then int64 (rand.Next() + 1) else y
                    let y1 = strToBigint (string y2 )
                    let divBint = divBnt x1 y1
                    let divNums = x / y2
                    let comp = strToBigint (string divNums)
                    Expect.isTrue (bntEqual divBint comp) "division works incorrectly"
                    
                testProperty "remainder test" <| fun (x : int64, y : int64) -> 
                    let x1 = strToBigint (string x)
                    let y2 =
                        if y = int64 0 then int64 1 else y
                    let y1 = strToBigint (string y2 )
                    let remBint = remBnt x1 y1
                    let remNums = x % y2
                    let comp = strToBigint (string remNums)
                    Expect.isTrue (bntEqual remBint comp) "remainder works incorrectly"
            ]