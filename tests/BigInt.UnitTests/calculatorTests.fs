namespace BigIntCalculatorTests

module CalculatorTests = 

    open Expecto
    open BigIntCalculator
    open Interpreter
    open BigIntFunctions
    open BigIntCalculatorTests.BigIntFunctionsTest

    // ensuring that variable declaring and printing works correctly
    let example =
        """
        let x = 9
        let y = -4
        let z = x + y
        let w = x * 7
        print x
        print y
        print z
        print w
        """ 
    let _, _, pD = run (parse example)
    printfn "%s" pD.[outputBuffer]
    printfn "Expected \n9\n-4\n5\n63"


    [<Tests>]
    let tests =
        testList "Calculations tests" [
            
            testCase "Remainder test" <| fun _ ->
                let calc = "let x = 10 % 6 + 23 % 7"
                let ansCalc = calculate (parse calc)
                Expect.equal "6" (bntToString ansCalc) "Remainder works incorrectly"
                
            testCase "Absolute value test" <| fun _ ->
                let calc = "let x = |-5| + |20| + | |-4| * |7| |"
                let ansCalc = calculate (parse calc)
                Expect.equal "53" (bntToString ansCalc) "Absolute value works incorrectly"
                
            testCase "Unary minus test" <| fun _ ->
                let calc = "let x = -485"
                let ansCalc = calculate (parse calc)
                Expect.equal "-485" (bntToString ansCalc) "Unary minus works incorrectly"
                
            testCase "Power test" <| fun _ ->
                let calc = "let x = 2^8 + 3^2"
                let ansCalc = calculate (parse calc)
                Expect.equal "265" (bntToString ansCalc) "Power works incorrectly"
                
            testCase "Binary test" <| fun _ ->
                let calc = "let x = &8"
                let anscalc = calculate (parse calc)
                Expect.equal "1000" (bntToString anscalc) "Transfer to binary works incorrectly"
            
            testCase "Parenthesis test" <| fun _ ->
                let calc = "let x = 25 + 2 * ((36 / 2) - 6 / (3 * 2))"
                let ansCalc = calculate (parse calc)
                Expect.equal "59" (bntToString ansCalc) "Parenthesis parsing works incorrectly"
                
            testCase "Calc test 1" <| fun _ ->
                let calc = "let x = 2 * 5 + 22 - (55 / 5) + 3"
                let ansCalc = calculate (parse calc)
                Expect.equal "24" (bntToString ansCalc) "Calc test 1 works incorrectly"
                
            testCase "Calc test 2" <| fun _ ->
                let calc = "let x = 10 + 45 - 23 * 3 + 4 - 9 + (9/5)"
                let ansCalc = calculate (parse calc)
                Expect.equal "-18" (bntToString ansCalc) "Calc test 1 works incorrectly"
                
            testCase "Calc test 3" <| fun _ ->
                let calc = "let x = 33 + 9 - (2*56) + 77 + (18/2)"
                let ansCalc = calculate (parse calc)
                Expect.equal "16" (bntToString ansCalc) "Calc test 1 works incorrectly"
        ]
