open Ast

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
  ("Found a unification that attempts to unify incompatible base types.")
let _UNIFICATION_ERROR_OTHER = Errors.Type_error 
  ("Discovered an edge-case constraint unification type error.")
let _UNIFICATION_OCCURS_CHECK_FAILURE = Errors.Type_error
  ("The occurs check for a type variable failed.")
let _UNIFICATION_ERROR_UNRESTR_TYPEVAR = Errors.Type_error
  ("The unification engine found an unrestricted type variable.")

(* *)
(* Types related to type checking. *)
type cctxOut = typ * typ TypR.t * TypC.t
type typCtx = typ TypR.t
type mergeFunction = typCtx -> typCtx -> typCtx * TypC.t
type checkFunction = binder -> typCtx -> typ * TypC.t
type tcOut = typ * TypC.t
                                                        
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
let unwrapSesh (st : typ) : sessTyp option = 
  match st with 
  | Session s -> Some s 
  | _ -> None

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

let linCheck (bind : binder) (ctx : typCtx)
             : typ * TypC.t =
  if TypR.mem bind ctx then (TypR.find bind ctx, (%.))
                       else raise _LINCHECK_VAR_NOT_FOUND
                           
let mixCheck (bind : binder) (ctx : typCtx)
             : typ * TypC.t =
  if TypR.mem bind ctx then (TypR.find bind ctx, (%.))
                       else let freshType = TypeVar (TyVar.fresh ()) in
                            (freshType, (%*) (Unrestricted freshType))
                           
let unrCheck (bind : binder) (ctx : typCtx)
             : typ * TypC.t =
  if TypR.mem bind ctx then (TypR.find bind ctx, (%.))
                       else let freshType = TypeVar (TyVar.fresh ()) in
                            (freshType, (%.))

(* *)
(* Typechecking section *)
let rec ccTc (mergeBranch : mergeFunction) (mergeSequence : mergeFunction) 
             (checkVariable : checkFunction) (tm : term) 
             : cctxOut =
  let typeCheck = ccTc mergeBranch mergeSequence checkVariable in
  match tm with
  | TConstant (CInteger _) -> 
      (Base Integer, TypR.empty, TypC.empty)
  | TConstant (CBoolean _) -> 
      (Base Boolean, TypR.empty, TypC.empty)
  | TVariable var -> 
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let freshBind = (&*) (var, freshTyp) in
    (freshTyp, freshBind, TypC.empty)
  | TUnit ->
      (Unit, TypR.empty, TypC.empty)
  (* Sequencing should follow equality: let _ = M in N === M;N with M:unit *)
  | TSequence (bndTm, coreTm) ->
    let (bndTyp, bndReq, bndCst) = typeCheck bndTm in
    let (coreTyp, coreReq, coreCst) = typeCheck coreTm in
    let (mergeReq, mergeCst) = mergeSequence bndReq coreReq in
    let fixedCst = (%*) (Equal (bndTyp, Unit)) in
    let outCst = bndCst %+ coreCst %+ mergeCst %+ fixedCst in
    (coreTyp, mergeReq, outCst)
  | TProduct (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = 
      typeCheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      typeCheck tm2 in
    let (tmMergeReq, tmMergeCst) = mergeSequence tm1Req tm2Req in
    let tmCst = tm1Cst %+ tm2Cst %+ tmMergeCst in
    (Product (tm1Typ, tm2Typ), tmMergeReq, tmCst)
  | TAbstract (bind, bndTyp, tm') ->
    let (tm'search, tm'Req, tm'Cst) = 
      typeCheck tm' in
    let (newTyp, bndCst) = checkVariable bind tm'Req in
    let correlatedCst = (%*) (Equal (bndTyp, newTyp)) in
    let outCst = tm'Cst %+ bndCst %+ correlatedCst in
    let outReq = tm'Req /< bind in
    (Arrow (bndTyp, tm'search), outReq, outCst)
  | TLinAbstract (bind, bndTyp, tm') ->
    let (tm'search, tm'Req, tm'Cst) = 
      typeCheck tm' in
    let (newTyp, bndCst) = checkVariable bind tm'Req in
    let correlatedCst = (%*) (Equal (newTyp, bndTyp)) in
    let ctxLinCst = genUnrestricted tm'Req in
    let outCst = tm'Cst %+ bndCst %+ ctxLinCst %+ correlatedCst in
    let outReq = tm'Req /< bind in
    (LinearArrow (bndTyp, tm'search), outReq, outCst)
  | TApplication (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = 
      typeCheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      typeCheck tm2 in
    let (req12, cst12) = mergeSequence tm1Req tm2Req in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let appCst = (%*) (Equal (tm1Typ, Arrow (tm2Typ, freshTyp))) in
    let outCst = tm1Cst %+ tm2Cst %+ appCst %+ cst12 in
    (freshTyp, req12, outCst)
  | TInL tm ->
    let (tmTyp, tmReq, tmCst) = typeCheck tm in
    let newTyp = Sum (tmTyp, TypeVar (TyVar.fresh ())) in
    (newTyp, tmReq, tmCst)
  | TInR tm ->
    let (tmTyp, tmReq, tmCst) = typeCheck tm in
    let newTyp = Sum (TypeVar (TyVar.fresh ()), tmTyp) in
    (newTyp, tmReq, tmCst)
  | TCase (tmScrutinee, tm1Bind, tm1, tm2Bind, tm2) ->
    let (tmScrutTyp, tmScrutReq, tmScrutCst) = typeCheck tmScrutinee in
    let (tm1Typ, tm1Req, tm1Cst) = typeCheck tm1 in
    let (_, tm2Req, tm2Cst) = typeCheck tm2 in
    let (sum1Typ, sum1Cst) = checkVariable tm1Bind tm1Req in
    let (sum2Typ, sum2Cst) = checkVariable tm2Bind tm2Req in
    let (brMergeReq, brMergeCst) = mergeBranch (tm1Req) (tm2Req) in
    let (seqMergeReq, seqMergeCst) = mergeSequence brMergeReq tmScrutReq in
    let fixedCst = (%+) ((%*) (Equal (sum1Typ, sum2Typ))) 
                        ((%*) (Equal (tmScrutTyp, Sum (sum1Typ, sum2Typ)))) in
    let outCst = tm1Cst %+ tm2Cst %+ sum1Cst %+ sum2Cst %+ tmScrutCst
                        %+ brMergeCst %+ seqMergeCst %+ fixedCst in
    (tm1Typ, seqMergeReq, outCst)
  | TLet (bnd, bndTm, coreTm) -> 
    let (bndTyp, bndReq, bndCst) = 
      typeCheck bndTm in
    let (coreTyp, coreReq, coreCst) = 
      typeCheck coreTm in
    let (newTyp, extensionCst) = checkVariable bnd coreReq in
    let correlatedCst = (%*) (Equal (newTyp, bndTyp)) in
    let (req12, cst12) = mergeSequence bndReq (coreReq /< bnd) in
    let outCst = bndCst %+ coreCst %+ extensionCst %+ cst12 %+ correlatedCst in
    let () = pp_TypC (Format.get_std_formatter ()) bndCst in
    let () = pp_TypC (Format.get_std_formatter ()) coreCst in
    let () = pp_TypC (Format.get_std_formatter ()) extensionCst in
    let () = pp_TypC (Format.get_std_formatter ()) cst12 in
    let () = pp_TypC (Format.get_std_formatter ()) correlatedCst in
    (coreTyp, req12, outCst)
  | TLetProduct (bndLeft, bndRight, bndTm, coreTm) ->
    let (bndTyp, bndReq, bndCst) = typeCheck bndTm in
    let (coreTyp, coreReq, coreCst) = typeCheck coreTm in
    let (mergeReq, mergeCst) = mergeSequence 
                               bndReq ((coreReq /< bndLeft) /< bndRight) in
    let (lProdTyp, lProdCst) = checkVariable bndLeft coreReq in
    let (rProdTyp, rProdCst) = checkVariable bndRight coreReq in
    let fixedCst = (%*) (Equal (bndTyp, Product (lProdTyp, rProdTyp))) in
    let outCst = bndCst %+ coreCst %+ mergeCst 
                        %+ lProdCst %+ rProdCst %+ fixedCst in
    (coreTyp, mergeReq, outCst)
  | TIf (tm1, tm2, tm3) ->
    let (tm1Typ, tm1Req, tm1Cst) = 
      typeCheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      typeCheck tm2 in
    let (tm3Typ, tm3Req, tm3Cst) = 
      typeCheck tm3 in
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
      typeCheck tm in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let newCst = (%*) (Equal (search, Arrow (freshTyp, freshTyp))) in
    let unrCst = genUnrestricted tmReq in
    let outCst = newCst %+ tmCst %+ unrCst in
    (freshTyp, tmReq, outCst)
  | TSend (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = typeCheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = typeCheck tm2 in
    let (mergeReq, mergeCst) = mergeSequence tm1Req tm2Req in
    let freshSesh = STypeVar (STyVar.fresh ()) in
    let receivedSesh = Session freshSesh in
    let fixedCst = (%*) (Equal (tm2Typ, (Session (Send (tm1Typ, freshSesh))))) in
    let outCst = tm1Cst %+ tm2Cst %+ mergeCst %+ fixedCst in
    (receivedSesh, mergeReq, outCst)
  | TReceive tm ->
    let (tmTyp, tmReq, tmCst) = typeCheck tm in
    let receivedFresh = TypeVar (TyVar.fresh ()) in
    let freshSesh = STypeVar (STyVar.fresh ()) in
    let receivedSesh = Session freshSesh in
    let fixedCst = (%*) 
                   (Equal (tmTyp, Session (Receive (receivedFresh, freshSesh)))) in
    let outCst = tmCst %+ fixedCst in
    (Product (receivedFresh, receivedSesh), tmReq, outCst)
  | TFork tm ->
    let (tmTyp, tmReq, tmCst) = typeCheck tm in
    let receivedSesh = TypeVar (TyVar.fresh ()) in
    let fixedCst = (%*) 
                   (Equal (tmTyp, 
                           LinearArrow (receivedSesh, Session SendEnd))) in
    let outCst = tmCst %+ fixedCst in
    (Dual receivedSesh, tmReq, outCst)
  | TWait tm ->
    let (tmTyp, tmReq, tmCst) = typeCheck tm in
    let fixedCst = (%*) (Equal (tmTyp, Session ReceiveEnd)) in
    let outCst = tmCst %+ fixedCst in
    (Unit, tmReq, outCst)
  | TBinOp (op, tm1, tm2) -> 
    let (tm1Typ, tm1Req, tm1Cst) = 
      typeCheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      typeCheck tm2 in
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
type livSubst = binder * typ

let rec occursCheck (t : typ) (checkSubject : typ) : bool = 
  match checkSubject with
  | LinearArrow (_, sbj2) | Arrow (_, sbj2) -> 
           if sbj2 = t then true
           else occursCheck t sbj2
  | Session sbj -> occursCheckSess t sbj
  | sbj -> t = sbj
and occursCheckSess (t : typ) (checkSubject : sessTyp) : bool =
  match t with
  | TypeVar _ ->
    (match checkSubject with
    | Send (head, cont) | Receive (head, cont) -> head = t 
                                                  || occursCheckSess t cont
    | SendChoice s | ReceiveChoice s -> List.exists (occursCheckSess t) s
    | _ -> false)
  | Session STypeVar sId ->
    (match checkSubject with
    | Send (_, cont) | Receive (_, cont) -> cont = STypeVar sId
    | SendChoice s | ReceiveChoice s -> List.exists ((=) (STypeVar sId)) s 
    | _ -> false)
  | _ -> false

let rec applySubst (substitution : livSubst) (examined : typ) : typ =
  let applyRec = applySubst substitution in
  let (search, subst) = substitution in
  match examined with
  | Base _ -> examined
  | Unit -> Unit
  | TypeVar _ -> if (TypeVar search) = examined then subst else examined
  | Product (t1, t2) -> Product (applyRec t1, applyRec t2)
  | Sum (t1, t2) -> Sum (applyRec t1, applyRec t2)
  | Arrow (t1, t2) -> Arrow (applyRec t1, applyRec t2)
  | LinearArrow (t1, t2) -> LinearArrow (applyRec t1, applyRec t2)
  | Session s -> Session (applySubstSession substitution s)
  | Dual t -> Dual (applyRec t)
and applySubstSession (substitution : livSubst) (examined : sessTyp) : sessTyp = 
  let (search, subst) = substitution in
  match examined with
  | STypeVar vId -> (match (unwrapSesh subst) with
                    | Some s -> if search = vId then s else examined
                    | None -> STypeVar vId)
  | Send (t, s) -> Send (applySubst substitution t, 
                         applySubstSession substitution s)
  | Receive (t, s) -> Receive (applySubst substitution t, 
                               applySubstSession substitution s)
  | SendChoice ss -> SendChoice 
                     (List.fold_right 
                       (fun el acc -> 
                         applySubstSession substitution el :: acc) 
                       ss [])
  | ReceiveChoice ss -> ReceiveChoice
                     (List.fold_right 
                       (fun el acc -> 
                         applySubstSession substitution el :: acc) 
                       ss [])
  | SendEnd -> SendEnd
  | ReceiveEnd -> ReceiveEnd

let substConstraint (substitution : livSubst) (c : TypC.elt) : TypC.elt = 
  match c with
  | Unrestricted t -> Unrestricted (applySubst substitution t)
  | Equal (t1, t2) -> Equal (applySubst substitution t1,
                             applySubst substitution t2)

let substConstraints (substitution : livSubst) 
                     (cs : TypC.elt list) : TypC.elt list =
  List.map (fun constr -> substConstraint substitution constr) cs

let closeSubsts (substitutions : livSubst list) (examined : typ) : typ = 
  List.fold_left 
  (fun subject substitution -> applySubst substitution subject) 
  examined substitutions 
                 
let rec checkUnrestr (constrTyp : typ) : bool =
  match constrTyp with
  | Base _ -> true
  | Unit -> true
  | Arrow (t1, t2) -> checkUnrestr t1 && checkUnrestr t2
  | LinearArrow (t1, t2) -> (not @@ checkUnrestr t1) && checkUnrestr t2
  | TypeVar _ -> false
  | Product (t1, t2) -> checkUnrestr t1 && checkUnrestr t2 
  | Sum (t1, t2) -> checkUnrestr t1 && checkUnrestr t2 
  | Session _ -> false
  | Dual _ -> false

let linearityCheck (constraintSubjects : typ list) : typ list =
  List.filter checkUnrestr constraintSubjects

let rec decomposeSessionEquality ((s1, s2) : sessTyp * sessTyp) 
                                 : TypC.elt list =
  match (s1, s2) with
  | (Send (h1, cont1), Send (h2, cont2)) 
  | (Receive (h1, cont1), Receive (h2, cont2)) -> 
      (Equal (h1, h2)) :: decomposeSessionEquality (cont1, cont2)
  | (STypeVar sId, s) | (s, STypeVar sId) -> 
      Equal (Session (STypeVar sId), Session s) :: []
  | (SendChoice ss1, SendChoice ss2) | (ReceiveChoice ss1, ReceiveChoice ss2) ->
    if List.length ss1 = List.length ss2 then
      List.fold_right (fun el acc -> decomposeSessionEquality el @ acc) 
        (List.combine ss1 ss2) []
    else raise (Errors.Type_error 
                  "Mismatched arity of supposedly equal choice session types.")
    (* TODO: Extract error *)
  | (s1, s2) -> if s1 = s2 then [] 
                else raise (Errors.Type_error "A case seems to have slipped through when decomposing equality constraints between two session types.")

let rec unifyEqualities (constraints : TypC.elt list) : livSubst list =
  match constraints with
  | [] -> []
  | (Unrestricted _) :: _ -> 
    raise _UNIFICATION_ERROR_UNRESTR_TYPEVAR
  | (Equal (t1, t2)) :: rest ->
    (match t1, t2 with
    (* Cases between a (session) type variable and a type are 
       the important ones, everything else just tries to 
       decompose complex types. *)
    | (Session STypeVar v, Session ty) | (Session ty, Session STypeVar v) -> 
      (* Is there any chance this could crash due to infinite recursion? A fix
         would be to do the occurs check first. *)
      let substituted = substConstraints (v, Session ty) rest in
      if occursCheckSess (Session (STypeVar v)) ty then
        let () = Format.printf "v: %a@;ty: %a@;" Format.pp_print_string v pp_sessTyp ty in
        raise _UNIFICATION_OCCURS_CHECK_FAILURE
      else 
        (v, Session ty) :: unifyEqualities substituted
    | (TypeVar v, typ) | (typ, TypeVar v) ->
      let substituted = substConstraints (v, typ) rest in
      (match typ with 
      | LinearArrow (t1,t2) | Arrow (t1,t2) -> 
        if occursCheck (TypeVar v) typ then 
        let () = Format.printf "t1: %a@;t2: %a AND %a@;" Format.pp_print_string v pp_typ t1 pp_typ t2 in
          raise _UNIFICATION_OCCURS_CHECK_FAILURE
        else (v, typ) :: unifyEqualities substituted 
      | _ -> (v, typ) :: unifyEqualities substituted
      )
    (* Complex type decomposition *)
    | (Arrow (t1, t2), Arrow (l1, l2))
    | (LinearArrow (t1, t2), LinearArrow (l1, l2)) ->
        unifyEqualities (Equal (t1, l1) :: Equal (t2, l2) :: rest)
    | (Session s1, Session s2) ->
        unifyEqualities ((decomposeSessionEquality (s1, s2)) @ rest)
    | (Product (t1, t2), Product (l1, l2)) -> 
        unifyEqualities (Equal (t1, l1) :: Equal (t2, l2) :: rest)
    | (t1, t2) -> if t1 = t2 then unifyEqualities rest
                  else raise _UNIFICATION_ERROR_INCOMPAT_BASE
    )

let reverseOrderUnrestricted (substitution : typConstraint) : bool =
  match substitution with
  | Unrestricted _ -> false
  | _ -> true

let closeUnrestrictedSubsts (constr : typConstraint) 
                            (substitutions : livSubst list) : typ =
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
  (checkVariable : checkFunction) (tm : term) 
  : cctxOut * (livSubst list) =
  let (_, _, cst) as out = ccTc mergeBranch mergeSequence checkVariable tm in  
  (out, resolveConstraints cst)

let bobTypecheck 
  (mergeBranch : mergeFunction) (mergeSequence : mergeFunction) 
  (checkVariable : checkFunction) (tm : term) : tcOut =
  let ((typ, _, cst), subst) = 
    checkResolve mergeBranch mergeSequence checkVariable tm in
  (closeSubsts subst typ, cst)

(* Utility function for pretty-printing type checker output *)
let pp_tcOut ?(verbose=false) (out : Format.formatter) ((t,c) : tcOut) =
  if verbose
  then Format.fprintf out "@[<hov>Term type:@ <%a>@;Under constraints:@ {%a}@]@."
                          pp_typ t pp_TypC c
  else Format.fprintf out "@[<hov>Term type:@ <%a>@]@." pp_typ t

(* Type checking function with linearity presets. *)
let typecheck (lb : linearityBase) (tm : term) : tcOut =
  match lb with
  | B_Linear -> bobTypecheck linBrMerge linSeqMerge linCheck tm
  | B_Mixed -> bobTypecheck mixBrMerge mixSeqMerge mixCheck tm
  | B_Unrestricted -> bobTypecheck unrMerge unrMerge unrCheck tm

