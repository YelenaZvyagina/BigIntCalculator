namespace BigIntCalculator
module AST =

    open BigIntCalculator.BigIntFunctions

    type VName = Var of string

    type Expression =
        | Num of bInt
        | NVar of VName
        | Sum of Expression * Expression
        | Sub of Expression * Expression
        | Div of Expression * Expression
        | Mul of Expression * Expression
        | Pow of Expression * Expression
        | Rem of Expression * Expression
        | Bin of Expression
        | Abs of Expression

    type Stmt =
        | VDecl of VName * Expression
        | Print of VName

    type Program = list<Stmt>