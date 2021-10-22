namespace bigIntCalculator

open bigIntCalculator
open AST
open bigIntFunctions

module toTree =
    
    let drawTree ast output = 
    
        let header = [ "digraph astGraph { \n"
                       "node [fillcolor = deeppink1, style = filled]; \n"
                     ]
        
        let footer = ["}"]
        
        let helper exprCount  labelName =
            let exprDecl = (sprintf "Expr%A" exprCount)+ " [label = " + (sprintf "%A" labelName )  + "]; \n"
            let expToX = (sprintf "Expr%A" exprCount ) + " -> " + (sprintf "Expr%A" (exprCount + 1) ) + "; \n"
            let expToY = (sprintf "Expr%A" exprCount ) + " -> " + (sprintf "Expr%A" (exprCount + 500) ) + "; \n"
            ([exprDecl] @ [expToX]), [expToY]
        
        let processExpr expr exprCount  = 
            let rec go expr res exprCount =
                match expr with
                | Num n ->
                    let nDecl = (sprintf "Expr%A" exprCount) + " [label = " + (sprintf "%A" (bntToString n) )  + "]; \n"
                    res @ [nDecl] 
                | NVar (Var n) ->
                    let exprDecl = (sprintf "Expr%A" exprCount)+ " [label = " + (sprintf "Nvar" )  + "]; \n"
                    let varDecl = (sprintf "Expr%A" (exprCount + 1) ) + " [label = " + (sprintf "%A" n )  + "]; \n"
                    let expToVd = (sprintf "Expr%A" exprCount ) + " -> " + (sprintf "Expr%A" (exprCount + 1) ) + "; \n"
                    res @ [varDecl] @ [exprDecl] @ [expToVd]
                | Sum (x, y) ->
                    let a = helper exprCount "Sum"
                    let xDecl = go x (res @ (fst a) ) (exprCount + 1)  
                    go y (xDecl @ (snd a) ) (exprCount + 500)
                | Sub (x, y) ->
                    let a = helper exprCount "Sub"
                    let xDecl = go x (res @ (fst a) ) (exprCount + 1)  
                    go y (xDecl @ (snd a) ) (exprCount + 500)
                | Div (x, y) ->
                    let a = helper exprCount "Div"
                    let xDecl = go x (res @ (fst a) ) (exprCount + 1)  
                    go y (xDecl @ (snd a) ) (exprCount + 500)
                | Mul (x, y) ->
                    let a = helper exprCount "Mul"
                    let xDecl = go x (res @ (fst a) ) (exprCount + 1)  
                    go y (xDecl @ (snd a) ) (exprCount + 500)
                | Pow (x, y) ->
                    let a = helper exprCount "Pow"
                    let xDecl = go x (res @ (fst a) ) (exprCount + 1)  
                    go y (xDecl @ (snd a) ) (exprCount + 500)
                | Rem (x, y) ->
                    let a = helper exprCount "Rem"
                    let xDecl = go x (res @ (fst a) ) (exprCount + 1)  
                    go y (xDecl @ (snd a) ) (exprCount + 500)
                | Bin x ->
                    let exprDecl = (sprintf "Expr%A" exprCount)+ " [label = " + (sprintf "Bin" )  + "]; \n"
                    let expToX = (sprintf "Expr%A" exprCount ) + " -> " + (sprintf "Expr%A" (exprCount + 1) ) + "; \n"
                    go x (res @ [exprDecl] @ [expToX]) (exprCount + 1) 
                | Abs x ->
                    let exprDecl = (sprintf "Expr%A" exprCount)+ " [label = " + (sprintf "Abs" )  + "]; \n"
                    let expToX = (sprintf "Expr%A" exprCount ) + " -> " + (sprintf "Expr%A" (exprCount + 1) ) + "; \n"
                    go x (res @ [exprDecl] @ [expToX]) (exprCount + 1) 
            go expr [] exprCount
            
        let rec processAst ast res vdCount printCount varCount expCount =
            match ast with
            | [] -> res
            | h :: t ->
                match h with
                | VDecl (Var varName, expr) ->
                    let vdDecl = (sprintf "Vdecl%A" vdCount) + " [label = " + "\"Vdecl\"" + "]; \n" 
                    let startToVd = "listStmt -> " + (sprintf "Vdecl%A" vdCount) + "; \n" 
                    let res1 = res @ [vdDecl] @ [startToVd]
                    
                    let varDecl = (sprintf "Var%A" varCount) + " [label = " + (sprintf "%A" varName )  + "]; \n" 
                    let vdToVar = (sprintf "Vdecl%A" vdCount) + " -> " + (sprintf "Var%A" varCount) + "; \n" 
                    let res2 = res1 @ [varDecl] @ [vdToVar]
                  
                    let exProc = processExpr expr expCount
                    
                    let vdToEx = (sprintf "Vdecl%A" vdCount) + " -> " + (sprintf "Expr%A" expCount) + "; \n"
                    
                    let resFinal = res2 @ [vdToEx] @ exProc
                    
                    let l = ast.Length * 10

                    processAst t resFinal (vdCount + 1) (printCount + 1) (varCount + 1) (expCount + l)
                    
                | Print (Var varName) ->
                    let printDecl = (sprintf "Print%A" printCount) + " [label = " + "\"Print\"" + "]; \n"
                    let startToPrt = "listStmt -> " + (sprintf "Print%A" printCount) + "; \n"
                    let res1 = res @ [printDecl] @ [startToPrt]
                    
                    let varDecl = (sprintf "Var%A" varCount) + " [label = " + (sprintf "%A" varName )  + "]; \n" 
                    let printToVar = (sprintf "Print%A" printCount) + " -> " + (sprintf "Var%A" varCount) + "; \n" 
                    let res2 = res1 @ [varDecl] @ [printToVar]
                    
                    processAst t res2 (vdCount + 1) (printCount + 1) (varCount + 1) (expCount + (ast.Length*10))
        let content = processAst ast [] 1 1 1 1
    
        System.IO.File.WriteAllLines (output, header @ content @ footer)
