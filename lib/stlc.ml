(* Co-contextual type variables with global ID counter *)
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

module STyVar = TyVar

(* STLC base types *)
type livBase = Integer | Boolean

(* STLC types *)
type livTyp = TypeVar of TyVar.t
            | Product of livTyp * livTyp
            | Sum of livTyp * livTyp
            | Base of livBase
            | Arrow of livTyp * livTyp 
            | LinearArrow of livTyp * livTyp
            | Unit
            | Session of sessTyp
            | Dual of livTyp
and sessTyp = STypeVar of STyVar.t
            | Send of livTyp * sessTyp
            | Receive of livTyp * sessTyp
            | SendChoice of sessTyp list
            | ReceiveChoice of sessTyp list
            | SendEnd
            | ReceiveEnd


(* Type universe for the purposes of constraints *)
(**********************************************************************)
(* type typeUniverse = ClassicType of livTyp | SessionType of sessTyp *)
(*                                                                    *)
(* let pp_typeUniverse (out : Format.formatter) (u : typeUniverse) =  *)
(*   match u with                                                     *)
(*   | ClassicType t -> Format.fprintf out "@[<hov>%a@]" pp_livTyp t  *)
(*   | SessionType s -> Format.fprintf out "@[<hov>%a@]" pp_sessTyp s *)
(**********************************************************************)

(* STLC binary operations *)
type livBinOp = Plus | Minus | Mult | Div
              | Lt | Le | Gt | Ge | Eq | Neq

(* STLC constants *)
type livConst = CInteger of int
              | CBoolean of bool

(* STLC variables *)
type livVar = string

(* Binders of lambdas *)
type livBinder = string

(* Terms of STLC *)
type livTerm = TConstant of livConst
             | TVariable of livVar
             | TAbstract of livBinder * livTyp * livTerm
             | TApplication of livTerm * livTerm
             | TLet of livBinder * livTerm * livTerm
             | TFix of livTerm * livTyp
             | TIf of livTerm * livTerm * livTerm
             | TLinAbstract of livBinder * livTyp * livTerm
             | TProduct of livTerm * livTerm
             | TLetProduct of livBinder * livBinder * livTerm * livTerm
             | TInL of livTerm
             | TInR of livTerm 
             | TCase of livTerm * livBinder * livTerm * livBinder * livTerm
             | TUnit
             | TSequence of livTerm * livTerm
             | TSend of livTerm * livTerm
             | TReceive of livTerm
             | TFork of livTerm
             | TWait of livTerm
             | TBinOp of livBinOp * livTerm * livTerm

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

(* Needs to be generalised to session types *)
type livConstraint = Unrestricted of livTyp 
                   | Equal of livTyp * livTyp


(* Sets of constraints *)
module TypC = Set.Make (struct
    type t = livConstraint
    let compare e1 e2 = match e1, e2 with
      | Unrestricted e1, Unrestricted e2
      | Equal (e1, _), Unrestricted e2
      | Unrestricted e1, Equal (e2, _)
      | Equal (e1, _), Equal (e2, _) -> Stdlib.compare e1 e2
  end)

(* Typing environments *)
module TypR = Map.Make (struct
    type t = livBinder
    let compare e1 e2 = String.compare e1 e2
  end)

(* Pretty printing *)
let pp_livBase (out : Format.formatter) (b : livBase) =
  match b with 
  | Integer -> Format.fprintf out "Int"
  | Boolean -> Format.fprintf out "Bool"

let rec pp_livTyp (out : Format.formatter) (t : livTyp) =
  match t with
  | TypeVar tv -> Format.fprintf out "%s" tv
  | Sum (t1, t2) -> Format.fprintf out "@[<hov>(%a+%a)@]"
                      pp_livTyp t1 pp_livTyp t2
  | Product (t1, t2) -> Format.fprintf out "@[<hov>(%a,%a)@]"
                          pp_livTyp t1 pp_livTyp t2
  | Base b -> (match b with
      | Integer -> Format.fprintf out "%s" "Int"
      | Boolean -> Format.fprintf out "%s" "Bool")
  | Arrow (t1, t2) -> Format.fprintf out "@[<hov>%a →@ %a@]" 
                        pp_livTyp t1 pp_livTyp t2
  | LinearArrow (t1, t2) -> Format.fprintf out "@[<hov>%a ⊸@ %a@]"
                              pp_livTyp t1 pp_livTyp t2
  | Session t -> pp_sessTyp out t
  | Dual t -> Format.fprintf out "@[<hov>‾%a@]" pp_livTyp t
  | Unit -> Format.print_string "()"
and pp_sessTyp (out : Format.formatter) (s : sessTyp) =
  match s with 
  | STypeVar s -> Format.print_string s
  | Send (t, s') -> Format.fprintf out "@[<hov>!%a.%a@]"
                      pp_livTyp t pp_sessTyp s' 
  | Receive (t, s') -> Format.fprintf out "@[<hov>?%a.%a@]"
                         pp_livTyp t pp_sessTyp s' 
  | SendChoice ss -> Format.fprintf out "@[<hov>⊕〈%a〉@]"
                       (Format.pp_print_list
                          ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
                          pp_sessTyp) ss
  | ReceiveChoice ss -> Format.fprintf out "@[<hov>&〈%a〉@]"
                          (Format.pp_print_list
                             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
                             pp_sessTyp) ss
  | SendEnd -> Format.print_string "end!"
  | ReceiveEnd -> Format.print_string "end?"

let pp_livConstraint (out : Format.formatter) (c : livConstraint) =
  match c with
  | Unrestricted (t) -> Format.fprintf out 
                        "@[<hov>\u{1D580} %a@]" pp_livTyp t
  | Equal (t1, t2) -> Format.fprintf out 
                      "@[<hov>%a@ \u{2250}@ %a@]" 
                      pp_livTyp t1 pp_livTyp t2

let pp_TypC (out : Format.formatter) (c : TypC.t) =
  let c_list = TypC.to_list c in
  Format.(pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ")) 
    pp_livConstraint out c_list

let pp_livBinOp (out : Format.formatter) (bin_op : livBinOp) =
  match bin_op with
  | Plus -> Format.fprintf out "+"
  | Minus -> Format.fprintf out "-"
  | Mult -> Format.fprintf out "×"
  | Div -> Format.fprintf out "/"
  | Lt -> Format.fprintf out "<"
  | Le -> Format.fprintf out "≤"
  | Gt -> Format.fprintf out ">"
  | Ge -> Format.fprintf out "≥"
  | Eq -> Format.fprintf out "="
  | Neq -> Format.fprintf out "≠"
 
let pp_livConst (out : Format.formatter) (const : livConst) = 
  match const with
  | CInteger i -> Format.fprintf out "⟪%a⟫" Format.pp_print_int i
  | CBoolean b -> Format.fprintf out "⟪%a⟫" Format.pp_print_bool b

let pp_livVar (out : Format.formatter) (var : livVar) = 
  Format.pp_print_string out var

let pp_livBinder (out : Format.formatter) (var : livVar) =
  Format.fprintf out "@[<hov>\u{0418}(%a)@]" Format.pp_print_string var

let rec pp_livTerm (out : Format.formatter) (t : livTerm) =
  match t with
  | TConstant c -> pp_livConst out c
  | TVariable v -> pp_livVar out v
  | TAbstract (binder, typ, tm) -> 
      Format.fprintf out "@[<hov>λ%a@ :@ %a@ .@ %a@]"
      pp_livBinder binder
      pp_livTyp typ
      pp_livTerm tm
  | TApplication (tm1, tm2) -> 
      Format.fprintf out "@[<hov>APP(%a@ %a)@]"
      pp_livTerm tm1 pp_livTerm tm2
  | TLet (binder, tmBound, tmCore) -> 
      Format.fprintf out "@[<hov>LET(%a@ =@ %a@ IN@ %a)@]"
      pp_livBinder binder
      pp_livTerm tmBound
      pp_livTerm tmCore
  | TFix (tm, typ) -> 
      Format.fprintf out "@[<hov>FIX(%a@ :@ %a)@]"
      pp_livTerm tm
      pp_livTyp typ
  | TIf (tmCnd, tm1, tm2) -> 
      Format.fprintf out "@[<hov>%a@ ?@ %a@ :@ %a)@]"
      pp_livTerm tmCnd
      pp_livTerm tm1
      pp_livTerm tm2
  | TLinAbstract (binder, typ, tm) -> 
      Format.fprintf out "@[<hov>%@λ%a@ :@ %a@ .@ %a@]"
      pp_livBinder binder
      pp_livTyp typ
      pp_livTerm tm
  | TProduct (tm1, tm2) -> 
      Format.fprintf out "@[<hov>PROD(%a,@ %a)@]"
      pp_livTerm tm1
      pp_livTerm tm2
  | TLetProduct (bndLeft, bndRight, tmBound, tmCore) -> 
      Format.fprintf out "@[<hov>PRODELIM(%a,@ %a,@ =@ %a@ IN@ %a@]"
      pp_livBinder bndLeft
      pp_livBinder bndRight
      pp_livTerm tmBound
      pp_livTerm tmCore
  | TInL tm -> Format.fprintf out "@[<hov>INL@ %a@]" pp_livTerm tm
  | TInR tm -> Format.fprintf out "@[<hov>INR@ %a@]" pp_livTerm tm
  | TCase (tmScrutinee, bndLeft, tmLeft, bndRight, tmRight) -> 
      Format.fprintf out "@[<hov>CASE@ %a@ IS@ %a@ ↦@ %a@ OR@ %a@ ↦@ %a@]"
      pp_livTerm tmScrutinee
      pp_livBinder bndLeft
      pp_livTerm tmLeft
      pp_livBinder bndRight
      pp_livTerm tmRight
  | TUnit -> Format.pp_print_string out "()"
  | TSequence (tm1, tm2) -> 
      Format.fprintf out "@[<hov>%a@ ;@ %a@]"
      pp_livTerm tm1
      pp_livTerm tm2
  | TSend (tmSubj, tmSesh) ->
      Format.fprintf out "@[<hov>SEND@ %a@ OVER@ %a@]"
      pp_livTerm tmSubj
      pp_livTerm tmSesh
  | TReceive tm -> Format.fprintf out "@[<hov>RECE@ %a@]" pp_livTerm tm
  | TFork tm -> Format.fprintf out "@[<hov>FORK@ %a@]" pp_livTerm tm
  | TWait tm -> Format.fprintf out "@[<hov>WAIT@ %a@]" pp_livTerm tm
  | TBinOp (op, tm1, tm2) -> 
      Format.fprintf out "@[<hov>BINOP@ %a@ %a@ %a@]"
      pp_livTerm tm1
      pp_livBinOp op
      pp_livTerm tm2
