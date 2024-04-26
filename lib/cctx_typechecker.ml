open Stlc

(* *)
(* Define errors relevant to type checking. *)
let _OP_ARG_ERR = Errors.Type_error 
  ("Failed to match argument of operator with expected type.")
let _NONSENS_ERR = Errors.Type_error 
  ("I am unsure how you managed to get here.") 
let _MERGE_EMPTY_VALUES = Errors.Type_error 
  ("Found two distinct type bindings in the type requirements that \newline
    have no types associated with them. Something must have \newline
    gone terribly wrong.")
let _LINMERGE_VARIABLE_REUSE = Errors.Type_error 
  ("Found duplicate type bindings when trying to merge contexts. \newline
    Are you sure you're using them linearly?")
let _LINMERGE_DIFFERING_BRANCH = Errors.Type_error
  ("Found two branches of control flow statements that do not have \newline
    matching variable-type assignments. Make sure both branches are linear.")
let _LINCHECK_VAR_NOT_FOUND = Errors.Type_error
  ("Found an unused variable in a linear context.")
let _UNIFICATION_ERROR_INF_LOOP = Errors.Type_error 
  ("Stumbled into infinite loop of type variables during unification.")
let _UNIFICATION_ERROR_BASE_ARROW = Errors.Type_error 
  ("Found a unification of a base type with an arrow type. \newline
    Non-functional applications perhaps?")
let _UNIFICATION_ERROR_INCOMPAT_BASE = Errors.Type_error 
  ("Found a unification that attempts to unify disjoint types of a sum.")
let _UNIFICATION_ERROR_OTHER = Errors.Type_error 
  ("Discovered an edge-case constraint unification type error.")

(* *)
(* Define types relevant to type checking. *)
type cctxOut = (livTyp * livTyp TypR.t * TypC.t)
type typCtx = livTyp TypR.t
type mergeFunction = typCtx -> typCtx -> typCtx * TypC.t
type checkFunction = livBinder -> livTyp -> typCtx -> TypC.t
    
(* *)
(* Define auxiliary functions that ease frequent operations. *)
let queryBind (binder : TypR.key) (reqSet : typCtx) = 
  TypR.find binder reqSet
(* The #< operator represents selecting (imagine # as a 'narrowing' operator) 
   a binder from a type context. The < represents the direction that the 
   selection operator # takes, so REQS #< BIND represents BIND getting 
   selected and piped into REQS.*)
let ( #< ) reqSet binder = queryBind binder reqSet

let removeBind (binder : TypR.key) (reqSet : typCtx) = 
  TypR.remove binder reqSet
(* Similarly to queryBind #<, the forward slash / symbol represents the removal
   of a certain binder from a set of requirements. *)
let ( /< ) reqSet binder = removeBind binder reqSet

(* The % represents a function working over TypC, the + represents the joining
   of two elements. That is SET1 %+ SET2 unions them. *)
let ( %+ ) cst1 cst2 = TypC.union cst1 cst2

(* The & represents a function working over TypR, the * represents the creation
   of a singleton element. *)
let ( &* ) (b, t) = TypR.singleton b t

(* See %+ and &* for an intuitive understanding of this operator. *)
let ( %* ) t = TypC.singleton t

(* The % represents a function working over TypC, the . represents the creation
   of an empty TypC. *)
let ( %. ) = TypC.empty

(* Just some unpack functions for cctxOut tuples. Really just a fancy 
   version of fst, snd, trd. *)
let unpackTyp ((search, _, _) : cctxOut) = search
let unpackReq ((_, req, _) : cctxOut) = req
let unpackCst ((_, _, cst) : cctxOut) = cst
  
(* Linear sequential merge *)
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

(* Linear branching control flow merge *)
(* According to Simon we don't actually need the dom(R1) = dom (R2) check? *)
let linBrMerge (req1 : typCtx) 
               (req2 : typCtx) : typCtx * TypC.t =
  let malformedMerge = TypR.bindings (TypR.merge
    (fun _ val1_opt val2_opt -> match val1_opt, val2_opt with
                                | (Some typ1, Some typ2) -> 
                                  Some (typ1, (%*) (typ1, typ2))
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

(* Unconstrained (non-linear) merge *)
let uncMerge (req1 : typCtx) 
             (req2 : typCtx) : typCtx * TypC.t =
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

(* My category theoretical spidey-sense is going off, I feel like the merges
   could be nicely generalised with monads and/or other type theoretical
   constructs. *)
(* Three argument version of a generalised merge. Nothing fancy, just
   adds results together. *)
(* Temporarily disabled, might get some use later so keep it. *)
(*
let genMerge3 (merge : typCtx -> typCtx -> typCtx * TypC.t)
              (req1 : typCtx) 
              (req2 : typCtx) 
              (req3 : typCtx) : typCtx * TypC.t =
  (let (req12, cst12) = merge req1 req2 in
   let (req123, cst12_3) = merge req12 req3 in
   let cst123 = cst12 %+ cst12_3 in
   (req123, cst123))
*)

(* Linear check function that determines a constraint based on a variable
   binder (with an associated context) and a type, which in usage would usually
   come from a second context. *)
let linCheck (bind : livBinder) (typ : livTyp) (ctx : typCtx)
             : TypC.t =
  if TypR.mem bind ctx then (%*) (ctx #< bind, typ) 
                       else raise _LINCHECK_VAR_NOT_FOUND
                           
(* Unconstrained (default) check function that determines constraints much like
   its linear counterpart, but in this case it does not err when it can't
   find the binder in the domain of the context. *)
let uncCheck (bind : livBinder) (typ : livTyp) (ctx : typCtx)
             : TypC.t =
  if TypR.mem bind ctx then (%*) (ctx #< bind, typ) 
                       else (%.)

(* *)
(* Quite a verbosely-written co-contextual typechecker. It produces a tuple of 
   type cctxContext which needs to have its constraints fed into a unification
   engine for the result to make sense and the type variables to disappear. *)
let rec ccTc (mergeBranch : mergeFunction) (mergeSequence : mergeFunction) 
             (checkVariable : checkFunction) (tm : livTerm) 
             : cctxOut =
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
    let (tm'search, tm'Req, tm'Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm' in
    let bndCst = (%*) (bndTyp, tm'Req #< bind) in
    let outCst = tm'Cst %+ bndCst in
    let outReq = tm'Req /< bind in
    (Arrow (bndTyp, tm'search), outReq, outCst)
  | TApplication (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = 
      ccTc mergeBranch mergeSequence checkVariable tm2 in
    let (req12, cst12) = mergeSequence tm1Req tm2Req in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let appCst = (%*) (tm1Typ, Arrow (tm2Typ, freshTyp)) in
    let outCst = tm1Cst %+ tm2Cst %+ appCst %+ cst12 in
    (freshTyp, req12, outCst)
  | TLet (bnd, bndTm, coreTm) -> 
    let (bndTyp, bndReq, bndCst) = 
      ccTc mergeBranch mergeSequence checkVariable bndTm in
    let (coreTyp, coreReq, coreCst) = 
      ccTc mergeBranch mergeSequence checkVariable coreTm in
    let extensionCst = (%*) (bndTyp, bndReq #< bnd) in
    let outReq = coreReq /< bnd in
    let outCst = bndCst %+ coreCst %+ extensionCst in
    (coreTyp, outReq, outCst)
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
    let predicateCst = (%*) (tm1Typ, Boolean) in
    let homoResultCst = (%*) (tm2Typ, tm3Typ) in
    let ifCst = predicateCst %+ homoResultCst in
    let inputCst = tm1Cst %+ tm2Cst %+ tm3Cst in
    let outCst = ifCst %+ inputCst %+ cst123 in
    (tm2Typ, req123, outCst)
  | TFix (tm, search) -> 
    let (_, tmReq, tmCst) = 
      ccTc mergeBranch mergeSequence checkVariable tm in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let newCst = (%*) (search, Arrow (freshTyp, freshTyp)) in
    let outCst = newCst %+ tmCst in
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

(* *)
(* Unification section *)
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

let substAppPairList (searchTerm, subsTerm : TypC.elt) (pairs : livSubst)
                     : livSubst =
  List.map (fun t -> substPair searchTerm subsTerm t) pairs

let substAppList (subs : livSubst) (typ : livTyp) : livTyp =
  List.fold_left (fun acc x -> substTyp (fst x) (snd x) acc)
                 typ subs

let rec unifyRec (pairList : TypC.elt list) : livSubst =
  match pairList with
  | (Boolean, Boolean) :: rest | (Integer, Integer) :: rest -> unifyRec rest
  | (TypeVar v, typ) :: rest | (typ, TypeVar v) :: rest ->
    let substitution = (TypeVar v, typ) in
    let substituted = substAppPairList substitution rest in
    (match typ with 
    Arrow _ -> 
      if occursCheck (TypeVar v) typ then raise _UNIFICATION_ERROR_INF_LOOP 
      else (TypeVar v, typ) :: unifyRec substituted 
    | _ -> (TypeVar v, typ) :: unifyRec substituted)
  | (Arrow (t1, t2), Arrow (l1, l2)) :: rest ->
    unifyRec ((t1, l1) :: (t2, l2) :: rest) (* Parallelisable with a merge. *)
  | (Arrow _, Integer) :: _ | (Integer, Arrow _) :: _ | (Arrow _, Boolean) :: _
  | (Boolean, Arrow _) :: _ -> raise _UNIFICATION_ERROR_BASE_ARROW
  | (Boolean, Integer) :: _ | (Integer, Boolean) :: _ ->
    raise _UNIFICATION_ERROR_INCOMPAT_BASE
  | [] -> []

(* Robinson's unification algorithm, it provides a unifying substitution. *)
let unify (pairs : TypC.t) : livSubst =
  let pairsList = TypC.elements pairs in
  unifyRec pairsList

(* Retrieves a most general substitution for the co-contextual result
   of checking tm. *)
let checkUnify 
  (mergeBranch : mergeFunction) 
  (mergeSequence : mergeFunction) 
  (checkVariable : checkFunction) 
  (tm : livTerm) 
  : cctxOut * livSubst =
  let (_, _, cst) as out = ccTc mergeBranch mergeSequence checkVariable tm in  
  (out, unify cst)

(* This calls on the cocontextual typechecker ccTc, then figures out the 
   most general substitution for the resulting constraints, then
   applies the substitution to the types given by ccTc. It produces
   a pair of type and constraints. *)
let bobTypecheck 
  (mergeBranch : mergeFunction) 
  (mergeSequence : mergeFunction) 
  (checkVariable : checkFunction) 
  (tm : livTerm) 
  : (livTyp * TypC.t) =
  let ((typ, _, cst), subst) = 
    checkUnify mergeBranch mergeSequence checkVariable tm in
  (substAppList subst typ, cst)

(* This discards the second projection of the output of bobTypecheck. *)
let bobTypecheckSimple 
  (mergeBranch : mergeFunction) 
  (mergeSequence : mergeFunction) 
  (checkVariable : checkFunction) 
  (tm : livTerm) 
  : livTyp =
  fst @@ bobTypecheck mergeBranch mergeSequence checkVariable tm
