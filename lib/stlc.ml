(* STLC type universe *)
type livTyp = Integer
            | Boolean
            | Arrow of livTyp * livTyp 
            [@@deriving show]

(* STLC binary operations *)
type livBinOp = Plus | Minus | Mult | Div
                | Lt | Le | Gt | Ge | Eq | Neq
                [@@deriving show]

(* STLC constants *)
type livConst = CInteger of int
              | CBoolean of bool
              [@@deriving show]

(* STLC variables *)
type livVar = string
              [@@deriving show]

(* Binders of lambdas *)
type livBinder = string
                 [@@deriving show]

(* Terms of STLC *)
type livTerm = TConstant of livConst
             | TVariable of livVar
             | TAbstract of livBinder * livTyp * livTerm
             | TApplication of livTerm * livTerm
             | TLet of livBinder * livTerm * livTerm
             | TBinOp of livBinOp * livTerm * livTerm
             (* Technically binary operators are constants in the language *)
             [@@deriving show]

(* Typing environment *)
type robEnv = (livVar * livTyp) list 
              [@@deriving show]
    
(* Values for evaluation *)
type gremVal = VInteger of int
             | VBoolean of bool
             | VClosure of livBinder * livTerm * gremEnv
(* Evaluation environment *)
and gremEnv = (livVar * gremVal) list
              [@@deriving show]
