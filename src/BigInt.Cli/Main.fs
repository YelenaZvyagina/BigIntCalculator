namespace BigIntCalculatorCli


module Main =

    open Argu
    open BigIntCalculator
    open ToTree
<<<<<<< HEAD
    open Interpreter
=======
>>>>>>> ea5ea2e8f07e07c92abd8411bcb0bd3ed8e663b2

    type CLIArguments =
        | InputFile of file:string
        | InputString of code:string
        | Compute
        | ToDot of output:string
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "File with code"
                | InputString _ -> "String of code" 
                | Compute -> "Return the result of interpretation of given code"
                | ToDot _ -> "Generates dot code of ast to the given file"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "BigInt interpreter")
        let results = parser.Parse(argv)
        let p = parser.ParseCommandLine argv
        if argv.Length = 0 || results.IsUsageRequested then parser.PrintUsage() |> printfn "%s"
        else 
            let input =
                if p.Contains(InputFile) then System.IO.File.ReadAllText (results.GetResult InputFile)
                elif p.Contains(InputString) then results.GetResult InputString
                else failwith "No input code given"
<<<<<<< HEAD
            let ast = parse input
            if p.Contains(Compute)
            then
                let _, _, pD = run ast
                printfn "%s" pD.[outputBuffer]
=======
            let ast = Interpreter.parse input
            if p.Contains(Compute)
            then
                let _, _, pD = Interpreter.run ast
                printfn "%s" pD.["print"]
>>>>>>> ea5ea2e8f07e07c92abd8411bcb0bd3ed8e663b2
            if p.Contains(ToDot) then toDot ast (results.GetResult ToDot)
        
        0