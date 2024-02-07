type livTyp = Integer of livTyp
            | Boolean of livTyp
            | Arrow of livTyp * livTyp 

type livConst = CInteger of int
              | CBoolean of bool

type livVar = string

type livBinder = string

type livTerm = TConstant of livConst
             | TVariable of livVar
             | TAbstract of livBinder * livTerm
             | TApplication of livTerm * livTerm
