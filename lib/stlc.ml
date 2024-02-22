type livTyp = Integer
            | Boolean
            | Arrow of livTyp * livTyp 
            [@@deriving show]

type livBinOp = Plus | Minus | Mult | Div
                | Lt | Le | Gt | Ge | Eq | Neq
                [@@deriving show]

type livConst = CInteger of int
              | CBoolean of bool
              [@@deriving show]

type livVar = string
              [@@deriving show]

type livBinder = string
                 [@@deriving show]

type livTerm = TConstant of livConst
             | TVariable of livVar
             | TAbstract of livBinder * livTyp * livTerm
             | TApplication of livTerm * livTerm
             | TBinOp of livBinOp * livTerm * livTerm
             (* Technically binary operators are constants in the language *)
             [@@deriving show]

type livEnv = (livVar * livTyp) list 
              [@@deriving show]
