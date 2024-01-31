type livTyp = Integer of livTyp
            | Boolean of livTyp
            | Arrow of livTyp * livTyp ;;

type livConst = CInteger of int
              | CBoolean of bool
              | CArith of int * int * int
              | CRelat of int * int * bool
              | BranchInt of bool * int * int * int
              | BranchBool of bool * bool * bool * bool

type livVar = VBase of livVar ;;

type livTerm = TConstant of livConst
             | TVariable of livVar
             | TAbstract of livVar * livTerm
             | TApplication of livTerm * livTerm ;;
