type livTyp = Integer
            | Boolean
            | Arrow of livTyp * livTyp 

type livBinOp = Plus | Minus | Mult | Div
                | Lt | Le | Gt | Ge | Eq | Neq

type livConst = CInteger of int
              | CBoolean of bool

type livVar = string

type livBinder = string

type livTerm = TConstant of livConst
             | TVariable of livVar
             | TAbstract of livBinder * livTyp * livTerm
             | TApplication of livTerm * livTerm
             | TBinOp of livBinOp * livTerm * livTerm
             (* Technically binary operators are constants in the language *)
