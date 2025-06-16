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
let _UNIFICATION_CHOICE_ARITY_MISMATCH = Errors.Type_error 
  ("Mismatched arity of supposedly equal choice session types.")
let _UNIFICATION_UNEXPECTED_DECOMPOSITION = Errors.Type_error 
  ("A case seems to have slipped through when decomposing equality constraints between two session types.")

(* *)
(* Types related to type checking. *)
type typCtx = typ TypR.t
type cctxOut = typ * typCtx * TypC.t
type mergeFunction = typCtx -> typCtx -> typCtx * TypC.t
type checkFunction = binder -> typCtx -> typ * TypC.t
type tcOut = typ * TypC.t
                                                        
(* *)
(* Type presets for easy input. *)
type linearityBase = B_Linear | B_Mixed | B_Unrestricted
type mergeType = M_Sequential | M_Branching
    
(* *)
(* Auxiliary functions. *)
let queryBind (binder : TypR.key) (reqSet : typCtx) = 
  TypR.find binder reqSet
let ( #< ) reqSet binder = queryBind binder reqSet
    
let queryBindOpt (binder : TypR.key) (reqSet : typCtx) = 
  TypR.find_opt binder reqSet
let ( #<? ) reqSet binder = queryBindOpt binder reqSet

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
    
let optionAnd (bo1 : bool option) (bo2 : bool option) : bool option =
  match (bo1, bo2) with
  | Some b1, Some b2 -> Some (b1 && b2)
  | _ -> None
    
let ( &&? ) b1 b2 = optionAnd b1 b2

let optionNot (bo : bool option) : bool option =
  match bo with
  | Some b -> Some (not b)
  | None -> None

let ( !? ) b = optionNot b

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
 
let rec merge (m : mergeType) (l : linearityBase) (inp : typCtx list)
    : typCtx * TypC.t =
  let mergeFun = 
    match m with 
    | M_Sequential -> 
      (match l with 
      | B_Linear -> linSeqMerge
      | B_Mixed -> mixSeqMerge
      | B_Unrestricted -> unrMerge)
    | M_Branching -> 
      (match l with 
      | B_Linear -> linBrMerge 
      | B_Mixed -> mixBrMerge 
      | B_Unrestricted -> unrMerge) in
  match inp with
  | [] -> (TypR.empty, (%.))
  | inp :: [] -> (inp, (%.))
  | inp1 :: inp2 :: inpTail ->
    let (req12, cst12) = mergeFun inp1 inp2 in
    let (req12t, cst12t) = merge m l (req12 :: inpTail) in
    let outCst = cst12 %+ cst12t in
    (req12t, outCst)

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
                            
let nCheck (l : linearityBase) (binds : binder list) (ctxs : typCtx list) 
           : typ list * TypC.t =
  let combined = List.combine binds ctxs in
  let checkFn (l : linearityBase) = match l with 
                                    | B_Linear -> linCheck 
                                    | B_Mixed -> mixCheck 
                                    | B_Unrestricted -> unrCheck in
  let typsAndCsts = List.fold_right (fun (bind, ctx) acc -> 
                                      (checkFn l bind ctx) :: acc) 
                                    combined [] in
  let outTyps = List.map (fun (t, _) -> t) typsAndCsts in
  let outCst = List.fold_right (fun (_, c) acc -> TypC.union c acc) 
               typsAndCsts (%.) in
  (outTyps, outCst)
  
let rec allEq (typs : typ list) : TypC.t =
  match typs with
  | t1 :: t2 :: ts -> TypC.union ((%*) @@ Equal (t1, t2)) (allEq ts)
  | _ -> (%.)

(* *)
(* Typechecking section *)
let rec ccTc (l : linearityBase) (tm : term) 
             : cctxOut =
  let typeCheck = ccTc l in
  let (mergeBranch, mergeSequence, checkVariable) = match l with 
                    | B_Linear -> (linBrMerge, linSeqMerge, linCheck) 
                    | B_Mixed -> (mixBrMerge, mixSeqMerge, mixCheck) 
                    | B_Unrestricted -> (unrMerge, unrMerge, unrCheck) in
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
    let (brMergeReq, brMergeCst) = 
      mergeBranch (tm1Req /< tm1Bind) (tm2Req /< tm2Bind) in
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
    let (tm1Typ, tm1Req, tm1Cst) = typeCheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = typeCheck tm2 in
    let (tm3Typ, tm3Req, tm3Cst) = typeCheck tm3 in
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
    let (_, tmReq, tmCst) = typeCheck tm in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let newCst = (%*) (Equal (search, Arrow (freshTyp, freshTyp))) in
    let unrCst = genUnrestricted tmReq in
    let outCst = newCst %+ tmCst %+ unrCst in
    (freshTyp, tmReq, outCst)
  | TSend (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = typeCheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = typeCheck tm2 in
    let (mergeReq, mergeCst) = mergeSequence tm1Req tm2Req in
    let freshSesh = TypeVar (TyVar.fresh ()) in
    let fixedCst = (%*) (Equal (tm2Typ, (Session (Send (tm1Typ, freshSesh))))) in
    let outCst = tm1Cst %+ tm2Cst %+ mergeCst %+ fixedCst in
    (freshSesh, mergeReq, outCst)
  | TReceive tm ->
    let (tmTyp, tmReq, tmCst) = typeCheck tm in
    let receivedFresh = TypeVar (TyVar.fresh ()) in
    let freshSesh = TypeVar (TyVar.fresh ()) in
    let fixedCst = (%*) 
                   (Equal (tmTyp, 
                           Session (Receive (receivedFresh, freshSesh)))) in
    let outCst = tmCst %+ fixedCst in
    (Product (receivedFresh, freshSesh), tmReq, outCst)
  | TOffer (coreTm, offerList) ->
    let brNMerge = merge M_Branching l in
    let (labels, contBinders, tmContinuations) = partition3 offerList in
    let (tmTyp, tmReq, tmCst) = typeCheck coreTm in
    let (tmTyps, tmReqs, tmCstsList) = partition3 @@ List.map typeCheck 
                                                          tmContinuations in
    let tmCsts = TypC.union_many tmCstsList in
    let (checkTyps, checkCst) = nCheck l contBinders tmReqs in
    let (brReq, brCst) = brNMerge @@ List.map (fun (sess, req) -> req /< sess) 
                                     (List.combine contBinders tmReqs) in 
    let (outReq, seqCst) = mergeSequence tmReq brReq in
    let outCst = seqCst %+ tmCst %+ brCst %+ checkCst %+ tmCsts 
                 %+ (allEq tmTyps) 
                 %+ (%*) (Equal (tmTyp, Session (SendChoice (List.combine labels checkTyps)))) in
    let outTyp = List.nth tmTyps 0 in
    (outTyp, outReq, outCst)
  | TSelect (_, _) -> raise (Errors.Type_error "Olivia did not implement this yet")
  | TFork tm ->
    let (tmTyp, tmReq, tmCst) = typeCheck tm in
    let receivedSesh = TypeVar (TyVar.fresh ()) in
    let fixedCst = (%*) 
                   (Equal (tmTyp, 
                           Arrow (receivedSesh, Session SendEnd))) in
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
    | Send (head, Session cont) | Receive (head, Session cont) -> head = t 
                                                  || occursCheckSess t cont
    | SendChoice s | OfferChoice s -> 
      List.exists (occursCheck t) (List.map (fun (_, el) -> el) s)
    | _ -> false)
  | _ -> false
    
let recoverSession (t : typ) : sessTyp = 
  match t with 
  | Session s -> s 
  | _ -> raise (Errors.Type_error {|Found a non-session type in a 
                                    selection type branch.|})

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
and applySubstSession (substitution : livSubst) (examined : sessTyp) 
                      : sessTyp = 
  match examined with
  | Send (t, cont) -> Send (applySubst substitution t, 
                             applySubst substitution cont)
  | Receive (t, cont) -> Receive (applySubst substitution t, 
                             applySubst substitution cont)
  | SendChoice ss -> 
    SendChoice 
      (List.fold_right
         (fun (vName, typ) acc -> 
            let sess = recoverSession typ in
            (vName, Session (applySubstSession substitution sess)) :: acc) 
         ss [])
  | OfferChoice ss ->
    OfferChoice 
      (List.fold_right
         (fun (vName, typ) acc -> 
            let sess = recoverSession typ in
            (vName, Session (applySubstSession substitution sess)) :: acc) 
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
                 
let rec isUnrestr (constrTyp : typ) : bool option =
  match constrTyp with
  | TypeVar _ -> None
  | Base _ -> Some true
  | Unit -> Some true
  | Arrow (t1, t2) -> isUnrestr t1 &&? isUnrestr t2
  | LinearArrow (t1, t2) -> (!? (isUnrestr t1)) &&? isUnrestr t2
  | Product (t1, t2) -> isUnrestr t1 &&? isUnrestr t2 
  | Sum (t1, t2) -> isUnrestr t1 &&? isUnrestr t2 
  | Session _ -> Some false
  | Dual _ -> Some false
                
let isUnrestrWeak (constrTyp : typ) : bool =
  match isUnrestr constrTyp with
  | Some b -> b
  | None -> false

let linearityCheck (constraintSubjects : typ list) : typ list =
  List.filter isUnrestrWeak constraintSubjects
    
let rec isClosed (t : typ) : bool =
  match t with
  | TypeVar _ -> false
  | Session s -> isClosedSess s
  | _ -> true
and isClosedSess (s : sessTyp) : bool = 
  match s with
  | Send (t1, t2) | Receive (t1, t2) -> 
    (match t2 with
    | Session s2 -> isClosed t1 && isClosedSess s2
    | _ -> 
      raise (Errors.Type_error {|Cannot determine continuation of session
                                 to be session-typed.|}))
  | SendChoice ss | OfferChoice ss ->
      List.for_all isClosedSess (List.map (fun (_, t) -> recoverSession t) ss)
  | SendEnd | ReceiveEnd -> true

let rec unifyEqualities (constraintList : TypC.elt list) 
                                 : livSubst list =
  match constraintList with
  | [] -> []
  | cst :: tail -> (
    match cst with
    | Equal (t1, t2) -> (
      if t1 = t2 then unifyEqualities tail else
      (match (t1, t2) with 
      | Product (t1, p1), Product (t2, p2) | Sum (t1, p1), Sum (t2, p2)
      | Arrow (t1, p1), Arrow (t2, p2) 
      | LinearArrow (t1, p1), LinearArrow (t2, p2) -> 
        unifyEqualities (Equal (t1, t2) :: Equal (p1, p2) :: tail)
          
      | t, Dual dt | Dual dt, t ->
        (match dt with
         | Session (Send (hd, tl)) ->
           unifyEqualities (Equal (t, Session (Receive (hd, Dual tl))) :: tail)
         | Session (Receive (hd, tl)) -> 
           unifyEqualities (Equal (t, Session (Send (hd, Dual tl))) :: tail)
         | Session (OfferChoice choices) ->
           let (binders, types) = List.split choices in
           let reconstructedTyp = List.combine 
                                    binders 
                                    (List.map (fun el -> Dual el) types) in
           unifyEqualities ((Equal (t, Session (SendChoice reconstructedTyp)) :: tail))
         | Session (SendChoice choices) ->
           let (binders, types) = List.split choices in
           let reconstructedTyp = List.combine 
                                    binders 
                                    (List.map (fun el -> Dual el) types) in
           unifyEqualities ((Equal (t, Session (OfferChoice reconstructedTyp)) :: tail))
         | Session SendEnd ->
           unifyEqualities (Equal (t, Session ReceiveEnd) :: tail)
         | Session ReceiveEnd ->
           unifyEqualities (Equal (t, Session SendEnd) :: tail)
         | TypeVar id -> 
           (match t with
           | TypeVar _ -> 
             raise (Errors.Type_error "Trying to switch dualities between two type variables.")
           | _ -> unifyEqualities (Equal (Dual t, TypeVar id) :: tail))
           (* Two typevars with duality may lead to nontermination *)
         | _ -> raise (Errors.Type_error "Found a non-session type in a session continuation.")
        )
      
      | Base t1, Base t2 -> 
        if t1 = t2 then unifyEqualities tail
        else raise (Errors.Type_error {|Base type mismatch.|}) (* Redundant *)
             
      | TypeVar tvId, t | t, TypeVar tvId ->
        let tv = TypeVar tvId in
        if occursCheck tv t then raise (Errors.Type_error "Occurs check failed")
        else let subst = (tvId, t) in
        let newCsts = substConstraints subst tail in
        subst :: unifyEqualities newCsts

      | Session s1, Session s2 ->
        (match (s1, s2) with
        | Send (headTyp1, contTyp1), Send (headTyp2, contTyp2)
        | Receive (headTyp1, contTyp1), Receive (headTyp2, contTyp2) ->
          unifyEqualities (Equal (headTyp1, headTyp2) 
                          :: Equal (contTyp1, contTyp2) :: tail)
        | SendEnd, SendEnd | ReceiveEnd, ReceiveEnd -> unifyEqualities tail
        | OfferChoice _, OfferChoice _ | SendChoice _, SendChoice _ ->
          raise (Errors.Type_error {|Branching choice unification has not been 
                                   implemented yet due to subtyping concerns.|})
        | _ -> raise (Errors.Type_error "Can't unify a session case. What's going on?")
        )
        
      | t1, t2 -> 
        Format.printf "Your types are %a and %a@;" 
          pp_typ t1 pp_typ t2; 
        Format.printf "Your constraints are %a@;" 
          pp_TypC (TypC.of_list tail); 
        raise (Errors.Type_error "Can't unify a case. What's going on?")
        )
      )
    | Unrestricted t -> (
        match (isUnrestr t) with
        | Some true -> unifyEqualities tail
        | Some false -> raise 
                     (Errors.Type_error {|Semantic unrestriction check failed|})
        | None -> unifyEqualities tail
      )
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

let checkResolve (l : linearityBase) (tm : term) 
  : cctxOut * (livSubst list) =
  let (_, _, cst) as out = ccTc l tm in  
  (out, resolveConstraints cst)

let bobTypecheck (l : linearityBase) (tm : term) : tcOut =
  let ((typ, _, cst), subst) = checkResolve l tm in
  (closeSubsts subst typ, cst)

(* Utility function for pretty-printing type checker output *)
let pp_tcOut ?(verbose=false) (out : Format.formatter) ((t,c) : tcOut) =
  if verbose
  then Format.fprintf out "@[<hov>Term type:@ <%a>@.Under constraints:@ {%a}@]\n@."
                          pp_typ t pp_TypC c
  else Format.fprintf out "@[<hov>Term type:@ <%a>@]\n@." pp_typ t

