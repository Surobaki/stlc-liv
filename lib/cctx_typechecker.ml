open Stlc

(* *)
(* Define errors relevant to type checking. *)
let _OP_ARG_ERR = Errors.Type_error 
  ("Failed to match argument of operator with expected type.")
let _NONSENS_ERR = Errors.Type_error 
  ("I am unsure how you managed to get here.") 
let _MERGE_EMPTY_VALUES = Errors.Type_error 
  ("Found two distinct type bindings in the type requirements that 
    have no types associated with them. Something must have 
    gone terribly wrong.")
let _LINMERGE_VARIABLE_REUSE = Errors.Type_error 
  ("Found duplicate type bindings when trying to merge contexts. 
    Are you sure you're using them linearly?")
let _LINMERGE_DIFFERING_BRANCH = Errors.Type_error
  ("Found two branches of control flow statements that do not have 
    matching variable-type assignments. Make sure both branches are linear.")
let _LINCHECK_VAR_NOT_FOUND = Errors.Type_error
  ("Found an unused variable in a linear context.")
let _UNIFICATION_ERROR_INF_LOOP = Errors.Type_error 
  ("Stumbled into infinite loop of type variables during unification.")
let _UNIFICATION_ERROR_BASE_ARROW = Errors.Type_error 
  ("Found a unification of a base type with an arrow type.
    Non-functional applications perhaps?")
let _UNIFICATION_ERROR_INCOMPAT_BASE = Errors.Type_error 
  ("Found a unification that attempts to unify disjoint types of a sum.")
let _UNIFICATION_ERROR_OTHER = Errors.Type_error 
  ("Discovered an edge-case constraint unification type error.")
let _UNIFICATION_OCCURS_CHECK_FAILURE = Errors.Type_error
  ("The occurs check for a type variable failed.")
let _UNIFICATION_ERROR_UNRESTR_TYPEVAR = Errors.Type_error
  ("The unification engine found an unrestricted type variable.")

(* *)
(* Types related to type checking. *)
type cctxOut = livTyp * livTyp TypR.t * TypC.t
type typCtx = livTyp TypR.t
type mergeFunction = typCtx -> typCtx -> typCtx * TypC.t
type checkFunction = livBinder -> livTyp -> typCtx -> TypC.t
type tcOut = livTyp * TypC.t
                                                        
(* *)
(* Type presets for easy input. *)
type linearityBase = B_Linear | B_Mixed | B_Unrestricted
    
(* *)
(* Auxiliary functions. *)
let queryBind (binder : TypR.key) (reqSet : typCtx) = 
  TypR.find binder reqSet
let ( #< ) reqSet binder = queryBind binder reqSet

let removeBind (binder : TypR.key) (reqSet : typCtx) = 
  TypR.remove binder reqSet
let ( /< ) reqSet binder = removeBind binder reqSet

let ( %+ ) cst1 cst2 = TypC.union cst1 cst2
let ( &* ) (b, t) = TypR.singleton b t
let ( %* ) t = TypC.singleton t
let ( %. ) = TypC.empty

let unpackTyp ((search, _, _) : cctxOut) = search
let unpackReq ((_, req, _) : cctxOut) = req
let unpackCst ((_, _, cst) : cctxOut) = cst
  
let linSeqMerge (req1 : typCtx) 
                (req2 : typCtx) : typCtx * TypC.t =
  let malformedMerge = TypR.bindings (TypR.merge
    (fun _ val1_opt val2_opt -> match val1_opt, val2_opt with
                                | (Some typ1, None) -> Some (typ1, (%.))
                                | (None, Some typ2) -> Some (typ2, (%.))
                                | (Some _, Some _) -> 
                                  raise _LINMERGE_VARIABLE_REUSE
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

let linBrMerge (req1 : typCtx) 
               (req2 : typCtx) : typCtx * TypC.t =
  let malformedMerge = TypR.bindings (TypR.merge
    (fun _ val1_opt val2_opt -> match val1_opt, val2_opt with
                                | (Some typ1, Some typ2) -> 
                                  Some (typ1, (%*) (Equal (typ1, typ2)))
                                | (Some _, None) | (None, Some _) ->
                                  raise _LINMERGE_DIFFERING_BRANCH
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

let unrMerge (req1 : typCtx) 
             (req2 : typCtx) : typCtx * TypC.t =
  let malformedMerge = TypR.bindings (TypR.merge 
    (fun _ val1_opt val2_opt -> match val1_opt, val2_opt with
                                | (Some typ1, Some typ2) -> 
                                    Some (typ1, (%*) (Equal (typ1, typ2))) 
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
 
let mixSeqMerge (req1 : typCtx)
                (req2 : typCtx) : typCtx * TypC.t =
  let malformedMerge = TypR.bindings (TypR.merge 
    (fun _ val1_opt val2_opt -> 
      match val1_opt, val2_opt with
      | (Some typ1, Some typ2) -> 
        Some (typ1, ((%+) ((%*) (Equal (typ1, typ2))) 
                          ((%*) (Unrestricted typ1))))
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

let mixBrMerge (req1 : typCtx)
               (req2 : typCtx) : typCtx * TypC.t =
  let malformedMerge = TypR.bindings (TypR.merge
    (fun _ val1_opt val2_opt -> 
      match val1_opt, val2_opt with
      | (Some typ1, Some typ2) -> 
        Some (typ1, (%*) (Equal (typ1, typ2)))
      | (Some typ, None) | (None, Some typ) ->
        Some (typ, (%*) (Unrestricted typ))
      | _ -> raise _MERGE_EMPTY_VALUES)
    req1 req2) in
  let fixedUpMerge = TypR.of_list 
                     (List.map (fun (key, (reqs, _)) ->
                       (key, reqs))
                     malformedMerge) in
  let extraConstraints = 
    List.fold_left 
    (fun acc (_, (_, cst)) -> if not (cst = (%.)) 
      then TypC.union acc cst
      else acc)
    TypC.empty malformedMerge in
    (fixedUpMerge, extraConstraints)

let genUnrestricted (ctx : typCtx) : TypC.t =
  TypC.of_list (List.map (fun (_, typ) -> Unrestricted typ) (TypR.to_list ctx))

let linCheck (bind : livBinder) (typ : livTyp) (ctx : typCtx)
             : TypC.t =
  if TypR.mem bind ctx then (%*) (Equal (ctx #< bind, typ))
                       else raise _LINCHECK_VAR_NOT_FOUND
                           
let mixCheck (bind : livBinder) (typ : livTyp) (ctx : typCtx)
             : TypC.t =
  if TypR.mem bind ctx then (%*) (Equal (ctx #< bind, typ))
                       else (%*) (Unrestricted typ)
                           
let unrCheck (bind : livBinder) (typ : livTyp) (ctx : typCtx)
             : TypC.t =
  if TypR.mem bind ctx then (%*) (Equal (ctx #< bind, typ))
                       else (%.)

(* *)
(* Typechecking section *)
let rec ccTc (mergeBranch : mergeFunction) (mergeSequence : mergeFunction) 
             (checkVariable : checkFunction) (tm : livTerm) 
             : cctxOut =
  match tm with
  | TConstant (CInteger _) -> 
      (Base Integer, TypR.empty, TypC.empty)
  | TConstant (CBoolean _) -> 
      (Base Boolean, TypR.empty, TypC.empty)
  | TVariable var -> 
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let freshBind = (&*) (var, freshTyp) in
    (freshTyp, freshBind, TypC.empty)
  | TAbstract (bind, bndTyp, tm') ->
    let (tm'search, tm'Req, tm'Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm' in
    let bndCst = checkVariable bind bndTyp tm'Req in
    let outCst = tm'Cst %+ bndCst in
    let outReq = tm'Req /< bind in
    (Arrow (bndTyp, tm'search), outReq, outCst)
  | TLinAbstract (bind, bndTyp, tm') ->
    let (tm'search, tm'Req, tm'Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm' in
    let bndCst = checkVariable bind bndTyp tm'Req in
    let ctxLinCst = genUnrestricted tm'Req in
    let outCst = tm'Cst %+ bndCst %+ ctxLinCst in
    let outReq = tm'Req /< bind in
    (Arrow (bndTyp, tm'search), outReq, outCst)
  | TApplication (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm2 in
    let (req12, cst12) = mergeSequence tm1Req tm2Req in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let appCst = (%*) (Equal (tm1Typ, Arrow (tm2Typ, freshTyp))) in
    let outCst = tm1Cst %+ tm2Cst %+ appCst %+ cst12 in
    (freshTyp, req12, outCst)
  | TLet (bnd, bndTm, coreTm) -> 
    let (bndTyp, bndReq, bndCst) = 
      ccTc mergeBranch mergeSequence checkVariable bndTm in
    let (coreTyp, coreReq, coreCst) = 
      ccTc mergeBranch mergeSequence checkVariable coreTm in
    let extensionCst = checkVariable bnd bndTyp coreReq in
    let (req12, cst12) = mergeSequence bndReq (coreReq /< bnd) in
    let outCst = bndCst %+ coreCst %+ extensionCst %+ cst12 in
    (coreTyp, req12, outCst)
  | TIf (tm1, tm2, tm3) ->
    let (tm1Typ, tm1Req, tm1Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm2 in
    let (tm3Typ, tm3Req, tm3Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm3 in
    let (branchReq, branchCst) = mergeBranch tm2Req tm3Req in
    let (seqReq, seqCst) = mergeSequence tm1Req branchReq in
    let (req123, cst123) = (seqReq, branchCst %+ seqCst) in
    let predicateCst = (%*) (Equal (tm1Typ, Base Boolean)) in
    let homoResultCst = (%*) (Equal (tm2Typ, tm3Typ)) in
    let ifCst = predicateCst %+ homoResultCst in
    let inputCst = tm1Cst %+ tm2Cst %+ tm3Cst in
    let outCst = ifCst %+ inputCst %+ cst123 in
    (tm2Typ, req123, outCst)
  | TFix (tm, search) -> 
    let (_, tmReq, tmCst) = 
      ccTc mergeBranch mergeSequence checkVariable tm in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let newCst = (%*) (Equal (search, Arrow (freshTyp, freshTyp))) in
    let unrCst = genUnrestricted tmReq in
    let outCst = newCst %+ tmCst %+ unrCst in
    (freshTyp, tmReq, outCst)
  | TBinOp (op, tm1, tm2) -> 
    let (tm1Typ, tm1Req, tm1Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm2 in
    let (tm12Req, mergeCst) = mergeSequence tm1Req tm2Req in
    let tm12Cst = tm1Cst %+ tm2Cst %+ mergeCst in
    (match op with
    | Plus | Minus | Mult | Div ->
      let arithCst1 = (%*) (Equal (tm1Typ, Base Integer)) in
      let arithCst2 = (%*) (Equal (tm2Typ, Base Integer)) in
      let arithCst12 = arithCst1 %+ arithCst2 in
      let outCst = tm12Cst %+ arithCst12 in
      (Base Integer, tm12Req, outCst)
    | Lt | Le | Gt | Ge ->
      let relCst1 = (%*) (Equal (tm1Typ, Base Integer)) in
      let relCst2 = (%*) (Equal (tm2Typ, Base Integer)) in
      let relCst12 = relCst1 %+ relCst2 in
      let outCst = tm12Cst %+ relCst12 in
      (Base Boolean, tm12Req, outCst)
    | Eq | Neq ->
      let eqCst = (%*) (Equal (tm1Typ, tm2Typ)) in
      let outCst = tm12Cst %+ eqCst in
      (Base Boolean, tm12Req, outCst)
    )

(* *)
(* Unification section *)
type livSubst = TyVar.t * livTyp

let rec occursCheck (typ : livTyp) (checkSubject : livTyp) : bool = 
  match checkSubject with
  | Arrow (_, sbj2) -> 
           if sbj2 = typ then true
           else occursCheck typ sbj2
  | sbj -> typ = sbj

let rec applySubst (substitution : livSubst) (examined : livTyp) : livTyp =
  let (search, subst) = substitution in
  match examined with
  | Base _ -> examined
  | TypeVar _ -> if (TypeVar search) = examined then subst else examined
  | Arrow (t1, t2) -> Arrow (applySubst substitution t1, 
                             applySubst substitution t2)

let substConstraint (substitution : livSubst) (c : TypC.elt) : TypC.elt = 
  let (search, subst) = substitution in
  match c with
  | Unrestricted t -> Unrestricted (applySubst (search, subst) t)
  | Equal (t1, t2) -> Equal (applySubst (search, subst) t1,
                             applySubst (search, subst) t2)

let substConstraints (substitution : livSubst) 
                     (cs : TypC.elt list) : TypC.elt list =
  List.map (fun constr -> substConstraint substitution constr) cs

let closeSubsts (substitutions : livSubst list) (examined : livTyp) : livTyp = 
  List.fold_left 
  (fun subject substitution -> applySubst substitution subject) 
  examined substitutions 
                 
let rec checkUnrestr (constrTyp : livTyp) : bool =
  match constrTyp with
  | Base _ -> true
  | Arrow (t1, t2) -> checkUnrestr t1 && checkUnrestr t2
  | TypeVar _ -> false

let linearityCheck (constraintSubjects : livTyp list) : livTyp list =
  List.filter checkUnrestr constraintSubjects

let rec unifyEqualities (constraints : TypC.elt list) : livSubst list =
  match constraints with
  | [] -> []
  | (Unrestricted _) :: _ -> 
    raise _UNIFICATION_ERROR_UNRESTR_TYPEVAR
  | (Equal (t1, t2)) :: rest ->
    (match t1, t2 with
    | (TypeVar v, typ) | (typ, TypeVar v) ->
      let substituted = substConstraints (v, typ) rest in
      (match typ with 
      | Arrow _ -> 
        if occursCheck (TypeVar v) typ then 
          raise _UNIFICATION_OCCURS_CHECK_FAILURE
        else (v, typ) :: unifyEqualities substituted 
      | _ -> (v, typ) :: unifyEqualities substituted
      )
    | (Arrow (t1, t2), Arrow (l1, l2)) ->
      unifyEqualities (Equal (t1, l1) :: Equal (t2, l2) :: rest)
    | (t1, t2) -> if t1 = t2 then unifyEqualities rest
                  else raise _UNIFICATION_ERROR_INCOMPAT_BASE
    )

let reverseOrderUnrestricted (substitution : livConstraint) : bool =
  match substitution with
  | Unrestricted _ -> false
  | _ -> true

let closeUnrestrictedSubsts (constr : livConstraint) 
                            (substitutions : livSubst list) : livTyp =
  match constr with 
  | Unrestricted typVar -> closeSubsts substitutions typVar
  | _ -> raise _UNIFICATION_ERROR_OTHER 

let resolveConstraints (constraints : TypC.t) : livSubst list =
  let constraintsList = TypC.elements constraints in
  let (equalityList, unrestrictionList) = 
    List.partition 
    reverseOrderUnrestricted
    constraintsList in
  let unified = unifyEqualities equalityList in
  let substituted = 
    List.map 
    (fun x -> closeUnrestrictedSubsts x unified)
    unrestrictionList in
  if (linearityCheck substituted) = substituted then unified
  else raise _UNIFICATION_ERROR_OTHER

let checkResolve 
  (mergeBranch : mergeFunction) (mergeSequence : mergeFunction) 
  (checkVariable : checkFunction) (tm : livTerm) 
  : cctxOut * (livSubst list) =
  let (_, _, cst) as out = ccTc mergeBranch mergeSequence checkVariable tm in  
  (out, resolveConstraints cst)

let bobTypecheck 
  (mergeBranch : mergeFunction) (mergeSequence : mergeFunction) 
  (checkVariable : checkFunction) (tm : livTerm) : tcOut =
  let ((typ, _, cst), subst) = 
    checkResolve mergeBranch mergeSequence checkVariable tm in
  (closeSubsts subst typ, cst)

(* Utility function for pretty-printing type checker output *)
let pp_tcOut ?(verbose=false) (out : Format.formatter) ((t,c) : tcOut) =
  if verbose
  then Format.fprintf out "@[<hov>Term type:@ <%a>@;Under constraints:@ {%a}@]@."
                          pp_livTyp t pp_TypC c
  else Format.fprintf out "@[<hov>Term type:@ <%a>@]@." pp_livTyp t

(* Type checking function with linearity presets. *)
let typecheck (lb : linearityBase) (tm : livTerm) : tcOut =
  match lb with
  | B_Linear -> bobTypecheck linBrMerge linSeqMerge linCheck tm
  | B_Mixed -> bobTypecheck mixBrMerge mixSeqMerge mixCheck tm
  | B_Unrestricted -> bobTypecheck unrMerge unrMerge unrCheck tm

