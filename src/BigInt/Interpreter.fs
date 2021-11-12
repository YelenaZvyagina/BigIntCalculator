namespace BigIntCalculator

open System
open System.Collections.Generic
open BigIntFunctions
open FSharp.Text.Lexing

[<AutoOpen>]
// refers to Interpreter but can be accessed externally
module CliColors =
    // module that describes error highlighting
    let complete = ConsoleColor.Magenta
    let green = ConsoleColor.Green
    let cyan = ConsoleColor.Cyan
    let yellow = ConsoleColor.Yellow
    let red = ConsoleColor.Red
    
    let errorHighlight line col msg lastToken =
        Console.ForegroundColor <- red 
        printf "Parsing failed at: "
        Console.ForegroundColor <- yellow
        printfn "line %A, column %A;" (line+1) (col+1)
        Console.ResetColor()
        printfn "Last token %A" lastToken
        Console.ForegroundColor <- cyan
        printfn "Message %A" msg
        Console.ResetColor()
        let errorString = (sprintf "Execution failed at: line %A, column %A. Last token %A" (line+1) (col+1) lastToken) 
        errorString
        

module Interpreter =
    
    let outputBuffer = "print"
    
    type Dicts = { VariablesDictionary: Dictionary<string, string>; InterpretedDictionary: Dictionary<AST.VName, AST.Expression> }
    
    let private newDataToConsole = Event<string>()

    let printed = newDataToConsole.Publish
    
    let rec processExpr (vDict:Dictionary<AST.VName,AST.Expression>) expr =
        match expr with
        | AST.Num n -> n
        | AST.NVar nv ->
            let data =
                try
                    vDict.[nv]
                with
                | _ -> failwithf "Variable %A is not declared." nv
            processExpr vDict data
        | AST.Sum (x, y) -> sumBint (processExpr vDict x) (processExpr vDict y)
        | AST.Sub (x, y) -> subBint (processExpr vDict x) (processExpr vDict y)
        | AST.Mul (x, y) -> multBnt (processExpr vDict x) (processExpr vDict y)
        | AST.Div (x, y) -> divBnt (processExpr vDict x) (processExpr vDict y)
        | AST.Rem (x, y) -> remBnt (processExpr vDict x) (processExpr vDict y)
        | AST.Pow (x, y) -> toPower (processExpr vDict x) (processExpr vDict y)
        | AST.Abs x -> absBnt (processExpr vDict x)
        | AST.Bin x -> toBinary (processExpr vDict x)

    let processStmt (vDict:Dictionary<AST.VName,AST.Expression>) printString stmt =
        match stmt with
        | AST.Print v ->
            let data =
                try
                    vDict.[v]
                with
                | _ -> failwithf "Variable %A is not declared." v
            match data with
            | AST.Num n ->
                let num = bntToString n
                let str = (if num.[0] = '+' then num.[1..] else num) + "\n"
                let str = (if num.[0] = '+' then num.[1..] else num) + "\n"
                newDataToConsole.Trigger str
                vDict, printString + str
            | _ ->
                failwithf "Num expected, got: %A" data
        | AST.VDecl(v,e) ->
            if vDict.ContainsKey v
            then vDict.[v] <- AST.Num (processExpr vDict e)
            else vDict.Add(v, AST.Num (processExpr vDict e))
            vDict, printString
            
    let runVariables (startDicts: Dicts) ast =
        let startDict = startDicts.VariablesDictionary
        let variableDict = startDicts.InterpretedDictionary
        let vD, _ = List.fold (fun (d1, ps) stmt -> processStmt d1 ps stmt) (variableDict, "") ast
        for i in vD.Keys do
            match vD.[i] with
            | AST.Num n ->
                match i with
                | AST.Var k -> startDict.[k] <- bntToString n
            | _ -> failwith "impossible case"
        let dicts = { VariablesDictionary = startDict; InterpretedDictionary = vD }
        dicts
        
    let runPrint ast =
        let vDict = Dictionary<_,_>()
        let varDict = Dictionary<_,_>()
        let vD, printString = List.fold (fun (d1, ps) stmt -> processStmt d1 ps stmt) (vDict, "") ast
        for i in vD.Keys do 
            match vD.[i] with
            | AST.Num n ->
                varDict.[string i] <- bntToString n
            | _ -> failwith "impossible case"
        printString

    let calculate (ast:AST.Stmt list) =
        match ast.[0] with
        | AST.VDecl (_, e) -> processExpr (Dictionary<_,_>()) e
        | _ -> failwithf "Variable declaration expected, got %A" ast.[0]

    let parse text =
        let lexbuf = LexBuffer<char>.FromString text
        try
            let parsed = Parser.start Lexer.tokenStream lexbuf
            parsed
        with errorMsg ->
            // Prints colored err message
            let pos = lexbuf.EndPos
            let line = pos.Line
            let column = pos.Column
            let message = errorMsg.Message
            let lastToken = String(lexbuf.Lexeme)
            let es = errorHighlight line column message lastToken
            failwithf "%A" es 
