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

(* STLC base types *)
type livBase = Integer 
             | Boolean
             [@@deriving show]

(* STLC type universe *)
type livTyp = TypeVar of TyVar.t
            | Base of livBase
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
             (* PCF Extensions *)
             | TLinAbstract of livBinder * livTyp * livTerm
             (* Linear abstraction *)
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
    
type livConstraint = Unrestricted of livTyp 
                   | Equal of livTyp * livTyp

module TypC = Set.Make (struct
  type t = livConstraint
  let compare e1 e2 = match e1, e2 with
                      | Unrestricted e1, Unrestricted e2
                      | Equal (e1, _), Unrestricted e2
                      | Unrestricted e1, Equal (e2, _)
                      | Equal (e1, _), Equal (e2, _) -> Stdlib.compare e1 e2
end)

module TypR = Map.Make (struct
  type t = livBinder
  let compare e1 e2 = String.compare e1 e2
end)

