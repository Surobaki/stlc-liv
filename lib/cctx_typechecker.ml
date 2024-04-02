open Stlc

let _OP_ARG_ERR = Errors.Type_error ("Failed to match argument of operator with expected type.")
let _NONSENS_ERR = Errors.Type_error ("I am unsure how you managed to get here.") 
let _MERGE_EMPTY_VALUES = Errors.Type_error ("Found two distinct type bindings in the type requirements that have no types associated with them. Something must have gone terribly wrong.")
let _UNIFICATION_ERROR_INF_LOOP = Errors.Type_error ("Stumbled into infinite loop of type variables during unification.")
let _UNIFICATION_ERROR_BASE_ARROW = Errors.Type_error ("Found a unification of a base type with an arrow type. Non-functional applications perhaps?")
let _UNIFICATION_ERROR_INCOMPAT_BASE = Errors.Type_error ("Found a unification that attempts to unify disjoint types of a sum.")
let _UNIFICATION_ERROR_OTHER = Errors.Type_error ("Discovered an edge-case constraint unification type error.")

type cctxOut = (livTyp * livTyp TypR.t * TypC.t)
    
let queryBind (binder : TypR.key) (reqSet : _ TypR.t) = 
  TypR.find binder reqSet
(* # is a selection of one key from the map.
   < is a piping from binder to map, datawise. *)
let ( #< ) reqSet binder = queryBind binder reqSet

let removeBind (binder : TypR.key) (reqSet : _ TypR.t) = 
  TypR.remove binder reqSet
(* / is a removal of one key from the map.
   < is a piping from binder to map, datawise. *)
let ( /< ) reqSet binder = removeBind binder reqSet

(* Set of equality constranints: %
   Union:                        + *)
let ( %+ ) cst1 cst2 = TypC.union cst1 cst2

(* Requirement set: &
   New element:     * *)
let ( &* ) (b, t) = TypR.singleton b t

(* Set of equality constranints: %
   New element:                  * *)
let ( %* ) t = TypC.singleton t
    
(* Unnecessary at the moment, keeping for linguistic consistency. *)
(* let ( &. ) = TypR.empty *)

(* Set of equality constranints: %
   Empty set:                    * *)
let ( %. ) = TypC.empty

(* Just some unpack functions for cctxOut tuples. Really just a fancy 
   version of fst, snd, trd. *)
let unpackTyp ((search, _, _) : cctxOut) = search
let unpackReq ((_, req, _) : cctxOut) = req
let unpackCst ((_, _, cst) : cctxOut) = cst

(* Requirement merge creates pair of requirements and constraints. *)
let reqMerge (req1 : 'a TypR.t) (req2 : 'a TypR.t) : 'a TypR.t * TypC.t =
  (* Feels very hacky... *)
  let malformedMerge = TypR.bindings (TypR.merge 
    (fun _ val1_opt val2_opt -> match val1_opt, val2_opt with
                                | (Some typ1, Some typ2) -> 
                                    Some (typ1, (%*) (typ1, typ2)) 
                                | (Some typ1, None) -> Some (typ1, (%.))
                                | (None, Some typ2) -> Some (typ2, (%.))
                                | _ -> raise _MERGE_EMPTY_VALUES)
                                     req1 req2) in
  let fixedUpMerge = TypR.of_list (List.map (fun (key, (reqs, _)) -> 
                                                 (key, reqs)) 
                                            malformedMerge) in
  let extraConstraints = List.fold_left 
                         (fun acc (_, (_, cst)) -> if not (cst = (%.)) 
                                                   then TypC.union acc cst
                                                   else acc)
                         TypC.empty malformedMerge in
  (fixedUpMerge, extraConstraints)

(* Feels like you could rework this into something monadic. *)
let reqMerge3 (req1 : 'a TypR.t) (req2 : 'a TypR.t) (req3 : 'a TypR.t) 
              : 'a TypR.t * TypC.t =
  let (req12, cst12) = reqMerge req1 req2 in
  let (req123, cst12_3) = reqMerge req12 req3 in
  let cst123 = cst12 %+ cst12_3 in
  (req123, cst123)

(* Requirement set: &
   Merge, union:    + *)
let ( &+ ) req1 req2 = 
  reqMerge req1 req2

(* Quite a verbosely-written co-contextual typechecker. It produces a tuple of 
   type cctxContext which needs to have its constraints fed into a unification
   engine for the result to make sense and the type variables to disappear. *)
let rec ccTypecheck (tm : livTerm) =
  match tm with
  | TConstant (CInteger _) -> 
      (Integer, TypR.empty, TypC.empty)
  | TConstant (CBoolean _) -> 
      (Boolean, TypR.empty, TypC.empty)
  | TVariable var -> 
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let freshBind = (&*) (var, freshTyp) in
    (freshTyp, freshBind, TypC.empty)
  | TAbstract (bind, bndTyp, tm') ->
    let (tm'search, tm'Req, tm'Cst) = ccTypecheck tm' in
    let bndCst = (%*) (bndTyp, tm'Req #< bind) in
    let outCst = tm'Cst %+ bndCst in
    let outReq = tm'Req /< bind in
    (Arrow (bndTyp, tm'search), outReq, outCst)
  | TApplication (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = ccTypecheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = ccTypecheck tm2 in
    let (req12, cst12) = reqMerge tm1Req tm2Req in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let appCst = (%*) (tm1Typ, Arrow (tm2Typ, freshTyp)) in
    let outCst = tm1Cst %+ tm2Cst %+ appCst %+ cst12 in
    (freshTyp, req12, outCst)
  | TLet (bnd, bndTm, coreTm) -> 
    let (bndTyp, bndReq, bndCst) = ccTypecheck bndTm in
    let (coreTyp, coreReq, coreCst) = ccTypecheck coreTm in
    let extensionCst = (%*) (bndTyp, bndReq #< bnd) in
    let outReq = coreReq /< bnd in
    let outCst = bndCst %+ coreCst %+ extensionCst in
    (coreTyp, outReq, outCst)
  | TIf (tm1, tm2, tm3) ->
    let (tm1Typ, tm1Req, tm1Cst) = ccTypecheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = ccTypecheck tm2 in
    let (tm3Typ, tm3Req, tm3Cst) = ccTypecheck tm3 in
    let (req123, cst123) = reqMerge3 tm1Req tm2Req tm3Req in
    let predicateCst = (%*) (tm1Typ, Boolean) in
    let homoResultCst = (%*) (tm2Typ, tm3Typ) in
    let ifCst = predicateCst %+ homoResultCst in
    let inputCst = tm1Cst %+ tm2Cst %+ tm3Cst in
    let outCst = ifCst %+ inputCst %+ cst123 in
    (tm2Typ, req123, outCst)
  | TFix (tm, search) -> 
    let (_, tmReq, tmCst) = ccTypecheck tm in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let newCst = (%*) (search, Arrow (freshTyp, freshTyp)) in
    let outCst = newCst %+ tmCst in
    (freshTyp, tmReq, outCst)
  | TBinOp (op, tm1, tm2) -> 
    let (tm1Typ, tm1Req, tm1Cst) = ccTypecheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = ccTypecheck tm2 in
    let (tm12Req, mergeCst) = tm1Req &+ tm2Req in
    let tm12Cst = tm1Cst %+ tm2Cst %+ mergeCst in
    (match op with
    | Plus | Minus | Mult | Div ->
      let arithCst1 = (%*) (tm1Typ, Integer) in
      let arithCst2 = (%*) (tm2Typ, Integer) in
      let arithCst12 = arithCst1 %+ arithCst2 in
      let outCst = tm12Cst %+ arithCst12 in
      (Integer, tm12Req, outCst)
    | Lt | Le | Gt | Ge ->
      let relCst1 = (%*) (tm1Typ, Integer) in
      let relCst2 = (%*) (tm2Typ, Integer) in
      let relCst12 = relCst1 %+ relCst2 in
      let outCst = tm12Cst %+ relCst12 in
      (Boolean, tm12Req, outCst)
    | Eq | Neq ->
      let eqCst = (%*) (tm1Typ, tm2Typ) in
      let outCst = tm12Cst %+ eqCst in
      (Boolean, tm12Req, outCst)
    )

(* Don't think I want to do Map.S with t = livTyp because left-to-right
   ordering appears to be important in the definition of the algorithm. As a
   result, each substitution from one type to another should be its own pair.
   If a map were to be used, the order of the keys would precede the order of
   the values, whereas the intention is to have each pair ordered separately. 
   By the way the type means substituting from the left projection of the pair
   to the right projection of the pair. Any ideas for making it a more in-
   trinsically understandable type or module? *)
type livSubst = (livTyp * livTyp) list

(* True if typ occurs in checkSubject as the codomain of a function type. *)
let rec occursCheck (typ : livTyp) (checkSubject : livTyp) : bool = 
  match checkSubject with
  | Arrow (_, sbj2) -> 
           if sbj2 = typ then true
           else occursCheck typ sbj2
  | sbj -> typ = sbj
           
let substTyp (search : livTyp) (subst : livTyp) (subj : livTyp) : livTyp =
  if search = subj then subst else subj

let substPair (search : livTyp) (subst : livTyp) (t1, t2) : TypC.elt = 
  let outT1 = if search = t1 then subst else t1 in
  let outT2 = if search = t2 then subst else t2 in
  (outT1, outT2)

(* Tries to substitute subsTerm for searchTerm if it finds it within the
   list of pairs that act as substitutions for our constraint equalities. 
   Does this once for every substitution found in the list. *)
(* Is this correct? Think about it. *)
let substAppPairList (searchTerm, subsTerm : TypC.elt) (pairs : livSubst)
                     : livSubst =
  List.map (fun t -> substPair searchTerm subsTerm t) pairs

let substAppList (subs : livSubst) (typ : livTyp) : livTyp =
  List.fold_left (fun typAcc sub -> if (fst sub) = typAcc then (snd sub)
                                    else typAcc)
                 typ subs

let rec unify_rec (pairList : TypC.elt list) : livSubst =
  match pairList with
  | (Boolean, Boolean) :: rest | (Integer, Integer) :: rest -> unify_rec rest
  | (TypeVar v, typ) :: rest | (typ, TypeVar v) :: rest ->
    let substitution = (TypeVar v, typ) in
    let substituted = substAppPairList substitution rest in
    (match typ with 
    Arrow _ -> 
      if occursCheck (TypeVar v) typ then raise _UNIFICATION_ERROR_INF_LOOP 
      else (TypeVar v, typ) :: unify_rec substituted 
    | _ -> (TypeVar v, typ) :: unify_rec substituted)
  | (Arrow (t1, t2), Arrow (l1, l2)) :: rest ->
    unify_rec ((t1, l1) :: (t2, l2) :: rest) (* Parallelisable with a merge. *)
  | (Arrow _, Integer) :: _ | (Integer, Arrow _) :: _ | (Arrow _, Boolean) :: _
  | (Boolean, Arrow _) :: _ -> raise _UNIFICATION_ERROR_BASE_ARROW
  | (Boolean, Integer) :: _ | (Integer, Boolean) :: _ ->
    raise _UNIFICATION_ERROR_INCOMPAT_BASE
  | [] -> []

(* Robinson's unification algorithm, it provides a unifying substitution. *)
let unify (pairs : TypC.t) : livSubst =
  let pairsList = TypC.elements pairs in
  unify_rec pairsList

let checkUnify (tm : livTerm) : cctxOut * livSubst =
  let (_, _, cst) as out = ccTypecheck tm in  
  (out, unify cst)

let bobTypecheck (tm : livTerm) : (livTyp * TypC.t) =
  let ((typ, _, cst), subst) = checkUnify tm in
  (substAppList subst typ, cst)

let bobTypecheckSimple (tm : livTerm) : livTyp =
  let (typ, _) = bobTypecheck tm in
  typ
