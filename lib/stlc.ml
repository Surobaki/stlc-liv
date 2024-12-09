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

module STyVar = TyVar

(* STLC base types *)
type livBase = Integer 
             | Boolean
[@@deriving show]

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
             | TProduct of livTerm * livTerm
             | TLetProduct of livBinder * livBinder * livTerm * livTerm
             | TInL of livTerm
             | TInR of livTerm 
             | TCase of livTerm * livBinder * livTerm * livBinder * livTerm
             | TUnit
             | TSequence of livTerm * livTerm
             (* Extended types *)
             | TSend of livTerm * livTerm
             | TReceive of livTerm
             | TFork of livTerm
             | TWait of livTerm
             (* Session statements *)
             | TBinOp of livBinOp * livTerm * livTerm
(* Binary operators *)
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
(* Needs to be generalised to session types *)

let pp_livConstraint (out : Format.formatter) (c : livConstraint) =
  match c with
  | Unrestricted (t) -> Format.fprintf out 
                        "@[<hov>\u{1D580} %a@]" pp_livTyp t
  | Equal (t1, t2) -> Format.fprintf out 
                      "@[<hov>%a@ \u{2250}@ %a@]" 
                      pp_livTyp t1 pp_livTyp t2

module TypC = Set.Make (struct
    type t = livConstraint
    let compare e1 e2 = match e1, e2 with
      | Unrestricted e1, Unrestricted e2
      | Equal (e1, _), Unrestricted e2
      | Unrestricted e1, Equal (e2, _)
      | Equal (e1, _), Equal (e2, _) -> Stdlib.compare e1 e2
  end)

let pp_TypC (out : Format.formatter) (c : TypC.t) =
  let c_list = TypC.to_list c in
  Format.(pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ")) 
    pp_livConstraint out c_list

module TypR = Map.Make (struct
    type t = livBinder
    let compare e1 e2 = String.compare e1 e2
  end)

