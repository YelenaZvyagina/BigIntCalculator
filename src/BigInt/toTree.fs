namespace BigIntCalculator

open BigIntCalculator
open AST
open BigIntFunctions

module ToTree =
    
    let toDot ast output = 
        
        let header = [ "digraph astGraph { " ]
        
        let footer = ["}"]
        
        let rec drawTree ast =
            
            let exprDecl label count =
                let s = (sprintf "\"expr%A\"" count) + ( sprintf " [label = %A ];" label )
                [s] 

            let exprTrans res label prevLabel count =
                let s = (sprintf "\"expr%A\"" prevLabel) + " -> " + (sprintf "\"expr%A\"" count) + " ;"
                res @ exprDecl label count @ [s]

            let mutable count = 0
            let rec expToDot expr res prevLabel =
                count <- count + 1
                match expr with
                | Num n -> exprTrans res (bntToString n) prevLabel count
                | NVar (Var v) ->
                    let res1 = exprTrans res "NVar" prevLabel count
                    count <- count + 1
                    let s = (sprintf "\"expr%A\"" (count - 1) ) + " -> " + (sprintf "\"expr%A\"" count) + "; "
                    res1 @ exprDecl (sprintf "%s" v) count @ [s]
                | Sum (x, y) ->
                    let res1 = exprTrans res "Sum" prevLabel count
                    let count1 = count
                    expToDot y (res1 @ expToDot x [""] count1) count1
                | Sub (x, y) ->
                    let res1 = exprTrans res "Sub" prevLabel count
                    let count1 = count
                    expToDot y (res1 @ expToDot x [""] count1) count1
                | Mul (x, y) ->
                    let res1 = exprTrans res "Mul" prevLabel count
                    let count1 = count
                    expToDot y (res1 @ expToDot x [""] count1) count1
                | Div (x, y) ->
                    let res1 = exprTrans res "Div" prevLabel count
                    let count1 = count
                    expToDot y (res1 @ expToDot x [""] count1) count1
                | Rem (x, y) ->
                    let res1 = exprTrans res "Rem" prevLabel count
                    let count1 = count
                    expToDot y (res1 @ expToDot x [""] count1) count1
                | Pow (x, y) ->
                    let res1 = exprTrans res "Pow" prevLabel count
                    let count1 = count
                    expToDot y (res1 @ expToDot x [""] count1) count1
                | Abs x ->
                    let res1 = exprTrans res "Abs" prevLabel count
                    expToDot x res1 count
                | Bin x ->
                    let res1 = exprTrans res "Bin" prevLabel count
                    expToDot x res1 count

            let rec go ast res =
                count <- count + 1
                match ast with
                | [] -> res
                | h :: t ->
                    match h with
                    | VDecl (Var v, e) ->
                        let res1 = exprTrans res "VDecl" 0 count
                        count <- count + 1
                        let res2 = exprTrans res1 (sprintf "%s" v) (count - 1) count
                        count <- count + 1
                        let res3 = res2 @ (expToDot e [""] (count - 2))
                        go t res3
                    | Print (Var v) ->
                        let newRes = exprTrans res "Print" 0 count
                        count <- count + 1
                        let newRes2 = exprTrans newRes (sprintf "%s" v) (count - 1) count
                        go t newRes2

            let start = exprDecl "list<Stmt>" 0
            
            go ast start
            
        let content = drawTree ast
        System.IO.File.WriteAllLines (output, header @ content @ footer)
