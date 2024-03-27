(* Co-contextual type variables *)
module TyVar = struct
  type t = string
  let source = ref 0
  
  let reset () = source := 0

  let fresh ?(prefix="_") () =
    let sym = !source in
    let () = incr source in
    prefix ^ (string_of_int sym)
  
  let pp = Format.pp_print_string
end
[@@deriving show]

(* STLC type universe *)
type livTyp = TypeVar of TyVar.t
            | Integer
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
             (* Base of STLC *)
             | TLet of livBinder * livTerm * livTerm
             | TFix of livTerm * livTyp
             | TIf of livTerm * livTerm * livTerm
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

module TypC = Set.Make (struct
  type t = livTyp * livTyp
  let compare e1 e2 = Stdlib.compare (fst e1) (fst e2)
end)

module TypR = Map.Make (struct
  type t = livBinder
  let compare e1 e2 = String.compare e1 e2
end)

