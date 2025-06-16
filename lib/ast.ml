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

(* Binders *)
type binder = string
type label = string

(* Base types *)
type baseType = Integer | Boolean

(* Types *)
type typ = TypeVar of TyVar.t
         | Product of typ * typ
         | Sum of typ * typ
         | Base of baseType
         | Arrow of typ * typ 
         | LinearArrow of typ * typ
         | Unit
         | Session of sessTyp
         | Dual of typ
(* Session types *)
and sessTyp = Send of typ * typ
            | Receive of typ * typ
            | SendChoice of (label * typ) list
            | OfferChoice of (label * typ) list
            | SendEnd
            | ReceiveEnd

(* Binary operations *)
type binOp = Plus | Minus | Mult | Div
           | Lt | Le | Gt | Ge | Eq | Neq

(* Constants *)
type constTerm = CInteger of int
               | CBoolean of bool

(* Variable names, might be useless *)
type varName = string

(* Terms *)
type term = 
  (* Simply Typed Lambda Calculus *)
  | TConstant of constTerm
  | TVariable of varName
  | TAbstract of binder * typ * term
  | TApplication of term * term
  | TBinOp of binOp * term * term
  (* Extensions *)
  | TLet of binder * term * term
  | TIf of term * term * term
  | TLinAbstract of binder * typ * term
  | TFix of term * typ
  (* Unit introduction and elimination *)
  | TUnit
  | TSequence of term * term
  (* Product introduction and elimination *)
  | TProduct of term * term
  | TLetProduct of binder * binder * term * term
  (* Sum introduction and elimination *)
  | TInL of term | TInR of term 
  | TCase of term * binder * term * binder * term
  (* Session types *)
  | TSend of term * term
  | TReceive of term
  | TFork of term
  | TWait of term
  | TOffer of term * (label * binder * term) list
           (* offer M in {l_i(x_i) |-> N_i} *)
  | TSelect of label * term
            (* select l in M *)

(* Typing environment *)
type typeEnv = (varName * typ) list 

(* Values for evaluation *)
type evalVal = VInteger of int
             | VBoolean of bool
             | VClosure of binder * term * evalEnv
(* Evaluation environment *)
and evalEnv = (varName * evalVal) list

(* Needs to be generalised to session types *)
type typConstraint = Unrestricted of typ 
                   | Equal of typ * typ

(* Sets of constraints *)
module TypCSet = Set.Make (struct
    type t = typConstraint
    let compare e1 e2 = match e1, e2 with
      | Unrestricted e1, Unrestricted e2
      | Equal (e1, _), Unrestricted e2
      | Unrestricted e1, Equal (e2, _) -> Stdlib.compare e1 e2
      | Equal (e11, e12), Equal (e21, e22) -> if e11 = e21 
                                             then Stdlib.compare e12 e22
                                             else Stdlib.compare e11 e21
  end)
 
module TypC = struct 
  include TypCSet
  let union_many = List.fold_left (union) empty
end

(* Typing environments *)
module TypR = Map.Make (struct
    type t = binder
    let compare e1 e2 = String.compare e1 e2
  end)
    
(* Auxiliary Functions *)
let partition2 (_input : ('a * 'b) list) : ('a list) * ('b list) =
  List.fold_right
  (fun (e1, e2) (acc1, acc2) -> (e1 :: acc1, e2 :: acc2))
  _input ([], [])

let partition3 (_input : ('a * 'b * 'c) list) 
               : ('a list) * ('b list) * ('c list) =
  List.fold_right 
  (fun (e1, e2, e3) (acc1, acc2, acc3) -> (e1 :: acc1, e2 :: acc2, e3 :: acc3))
  _input ([], [], [])

(* Pretty printing *)
let pp_baseType (out : Format.formatter) (b : baseType) =
  match b with 
  | Integer -> Format.fprintf out "Int"
  | Boolean -> Format.fprintf out "Bool"

let rec pp_typ (out : Format.formatter) (t : typ) =
  match t with
  | TypeVar tv -> Format.fprintf out "%s" tv
  | Sum (t1, t2) -> Format.fprintf out "(%a +@ %a)"
                      pp_typ t1 pp_typ t2
  | Product (t1, t2) -> Format.fprintf out "(%a *@ %a)"
                          pp_typ t1 pp_typ t2
  | Base b -> pp_baseType out b
  | Arrow (t1, t2) -> Format.fprintf out "%a →@ %a" 
                        pp_typ t1 pp_typ t2
  | LinearArrow (t1, t2) -> Format.fprintf out "%a ⊸@ %a"
                              pp_typ t1 pp_typ t2
  | Session t -> pp_sessTyp out t
  | Dual t -> Format.fprintf out "‾(%a)" pp_typ t
  | Unit -> Format.print_string "𝟙"
and pp_sessTyp (out : Format.formatter) (s : sessTyp) =
  match s with 
  | Send (t, s') -> Format.fprintf out "! %a@;<1 2>%a"
                      pp_typ t pp_typ s' 
  | Receive (t, s') -> Format.fprintf out "? %a@;<1 2>%a"
                         pp_typ t pp_typ s' 
  | SendChoice ss -> let (labels, typs) = partition2 ss in
    Format.fprintf out "&〈{%a :@;<1 2>%a}〉"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
        pp_typ) typs
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
        Format.pp_print_string) labels
  | OfferChoice ss -> let (labels, typs) = partition2 ss in
    Format.fprintf out "⊕〈{%a :@;<1 2>%a}〉"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
        pp_typ) typs
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
        Format.pp_print_string) labels
  | SendEnd -> Format.print_string ".end!"
  | ReceiveEnd -> Format.print_string ".end?"

let pp_typConstraint (out : Format.formatter) (c : typConstraint) =
  match c with
  | Unrestricted t -> Format.fprintf out 
                        "\u{1D580}(%a)" pp_typ t
  | Equal (t1, t2) -> Format.fprintf out 
                      "%a \u{2250}@ %a" 
                      pp_typ t1 pp_typ t2

let pp_TypC (out : Format.formatter) (c : TypC.t) =
  let c_list = TypC.to_list c in
  Format.fprintf out "{%a}" 
    (Format.(pp_print_list 
      ~pp_sep:(fun ppf () -> 
        fprintf ppf ",@ ")) 
      pp_typConstraint) c_list
    
let pp_typBinding (out : Format.formatter) ((s, t) : string * typ) =
  Format.fprintf out "%a ↦@ %a" Format.pp_print_string s pp_typ t

let pp_TypR (out : Format.formatter) (r : typ TypR.t) =
  let r_list = TypR.to_list r in
  Format.(pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ")) 
    pp_typBinding out r_list

let pp_binOp (out : Format.formatter) (bin_op : binOp) =
  match bin_op with
  | Plus -> Format.fprintf out "+" | Minus -> Format.fprintf out "-"
  | Mult -> Format.fprintf out "×" | Div -> Format.fprintf out "/"
  | Lt -> Format.fprintf out "<"   | Le -> Format.fprintf out "≤"
  | Gt -> Format.fprintf out ">"   | Ge -> Format.fprintf out "≥"
  | Eq -> Format.fprintf out "="   | Neq -> Format.fprintf out "≠"
 
let pp_constTerm (out : Format.formatter) (const : constTerm) = 
  match const with
  | CInteger i -> Format.fprintf out "⟪%a⟫" Format.pp_print_int i
  | CBoolean b -> Format.fprintf out "⟪%a⟫" Format.pp_print_bool b

let pp_varName (out : Format.formatter) (var : varName) = 
  Format.pp_print_string out var

let pp_binder (out : Format.formatter) (var : varName) =
  Format.fprintf out "\u{0418}(%a)" Format.pp_print_string var

let rec pp_term (out : Format.formatter) (t : term) =
  match t with
  | TConstant c -> pp_constTerm out c
  | TVariable v -> pp_varName out v
  | TAbstract (binder, typ, tm) -> 
      Format.fprintf out "λ%a :@ %a .@ %a"
      pp_binder binder
      pp_typ typ
      pp_term tm
  | TApplication (tm1, tm2) -> 
      Format.fprintf out "APP(%a TO@ %a)"
      pp_term tm1 pp_term tm2
  | TLet (binder, tmBound, tmCore) -> 
      Format.fprintf out "LET(%a =@ @[%a IN@ %a)@]"
      pp_binder binder
      pp_term tmBound
      pp_term tmCore
  | TFix (tm, typ) -> 
      Format.fprintf out "FIX(%a :@ %a)"
      pp_term tm
      pp_typ typ
  | TIf (tmCnd, tm1, tm2) -> 
      Format.fprintf out "(%a ?@ %a :@ %a)"
      pp_term tmCnd
      pp_term tm1
      pp_term tm2
  | TLinAbstract (binder, typ, tm) -> 
      Format.fprintf out "%@λ%a@ :@ %a@ .@ %a"
      pp_binder binder
      pp_typ typ
      pp_term tm
  | TProduct (tm1, tm2) -> 
      Format.fprintf out "PROD(%a,@ %a)"
      pp_term tm1
      pp_term tm2
  | TLetProduct (bndLeft, bndRight, tmBound, tmCore) -> 
      Format.fprintf out "PRODELIM(%a,@ %a,@ =@ %a@ IN@ %a"
      pp_binder bndLeft
      pp_binder bndRight
      pp_term tmBound
      pp_term tmCore
  | TInL tm -> Format.fprintf out "INL %a" pp_term tm
  | TInR tm -> Format.fprintf out "INR %a" pp_term tm
  | TCase (tmScrutinee, bndLeft, tmLeft, bndRight, tmRight) -> 
      Format.fprintf out "CASE@ %a@ IS@ %a@ ↦@ %a@ OR@ %a@ ↦@ %a"
      pp_term tmScrutinee
      pp_binder bndLeft
      pp_term tmLeft
      pp_binder bndRight
      pp_term tmRight
  | TUnit -> Format.pp_print_string out "()"
  | TSequence (tm1, tm2) -> 
      Format.fprintf out "%a@ ;@ %a"
      pp_term tm1
      pp_term tm2
  | TSend (tmSubj, tmSesh) ->
      Format.fprintf out "SEND@ %a@ OVER@ %a"
      pp_term tmSubj
      pp_term tmSesh
  | TReceive tm -> Format.fprintf out "RECE@ %a" pp_term tm
  | TOffer (sessTm, tripleList) ->
      pp_offer out sessTm tripleList
  | TSelect (bind, tm) ->
      Format.fprintf out "SELECT@ %a@ FROM@ %a"
      Format.pp_print_string bind
      pp_term tm
  | TFork tm -> Format.fprintf out "FORK@ %a" pp_term tm
  | TWait tm -> Format.fprintf out "WAIT@ %a" pp_term tm
  | TBinOp (op, tm1, tm2) -> 
      Format.fprintf out "BINOP@ %a@ %a@ %a"
      pp_term tm1
      pp_binOp op
      pp_term tm2
      
and pp_case_elem (out : Format.formatter) ((key, value) : string * term) =
  Format.fprintf out "%a@ ↦@ %a"
  Format.pp_print_string key
  pp_term value
  
and pp_case_continuation_elem (out : Format.formatter)
                              ((label, cont, tm) : label * binder * term) =
  Format.fprintf out "%a@ (%a)@ ↦@ %a"
  Format.pp_print_string label
  Format.pp_print_string cont
  pp_term tm

and pp_case (out : Format.formatter) (keyvalues : (binder * term) list) =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
  pp_case_elem out keyvalues

and pp_case_continuation (out : Format.formatter)
                         (triples : (label * binder * term) list) =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
    pp_case_continuation_elem out triples

and pp_offer (out : Format.formatter) (sess : term) 
             (triples : (label * binder * term) list) =
  Format.fprintf out "OFFER@ %a@ {%a}"
  pp_term sess
  pp_case_continuation triples

let pp_typeEnvSingleton (out : Format.formatter) ((b, t) : binder * typ) =
  Format.fprintf out "RENV(%a,@ %a)" 
  pp_binder b
  pp_typ t

let pp_typeEnv (out : Format.formatter) (env : typeEnv) =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
  pp_typeEnvSingleton out env

let rec pp_evalVal (out : Format.formatter) (value : evalVal) =
  match value with
  | VInteger i -> Format.pp_print_int out i
  | VBoolean b -> Format.pp_print_bool out b
  | VClosure (bind, tm, env) -> Format.fprintf out "CLOS<%a,@ %a,@ %a>"
                                pp_binder bind
                                pp_term tm
                                pp_evalEnv env
and pp_evalEnv (out : Format.formatter) (env : evalEnv) =
  let pp_evalEnvEl out (bind, value) = 
    Format.fprintf out "(%a,@ %a)"
    pp_binder bind
    pp_evalVal value
  in
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
  pp_evalEnvEl out env
                           
