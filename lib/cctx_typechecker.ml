open Stlc

let _OP_ARG_ERR = Errors.Type_error ("Failed to match argument of operator with expected type.")
let _NONSENS_ERR = Errors.Type_error ("I am unsure how you managed to get here.") 

module StringSet = Set.Make (String)

type cctxOut = (livTyp * livTyp TypR.t * TypC.t)
    
let queryBind binder reqSet = TypR.find binder reqSet
(* # is a selection of one key from the map.
   < is a piping from binder to map, datawise. *)
let ( #< ) reqSet binder = queryBind binder reqSet

let removeBind binder reqSet = TypR.remove binder reqSet
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
    
(* Unnecessary at the moment *)
(* let ( &. ) = TypR.empty *)

let ( %. ) = TypC.empty
    
let unpackTyp triple = match triple with (typ, _, _) -> typ

let unpackReq triple = match triple with (_, req, _) -> req

let unpackCst triple = match triple with (_, _, cst) -> cst

(* Requirement merge creates pair of requirements and constraints. *)
let reqMerge req1 req2 =
  (* Feels very hacky... *)
  let malformedMerge = TypR.bindings (TypR.merge 
    (fun _ val1_opt val2_opt -> match val1_opt, val2_opt with
                                | (Some typ1, Some typ2) -> 
                                    Some (typ1, (%*) (typ1, typ2)) 
                                | (Some typ1, None) -> Some (typ1, (%.))
                                | (None, Some typ2) -> Some (typ2, (%.))
                                | _ -> raise _NONSENS_ERR)
                                     req1 req2) in
  let fixedUpMerge = TypR.of_list (List.map (fun (key, (reqs, _)) -> 
                                                 (key, reqs)) 
                                            malformedMerge) in
  let extraConstraints = List.fold_left 
                         (fun acc (_, (_, cst)) -> TypC.union acc cst)
                         TypC.empty malformedMerge in
  (fixedUpMerge, extraConstraints)

let reqMerge3 req1 req2 req3 =
  let (req12, cst12) = reqMerge req1 req2 in
  let (req123, cst12_3) = reqMerge req12 req3 in
  let cst123 = cst12 %+ cst12_3 in
  (req123, cst123)

(* Requirement set: &
   Merge, union:    + *)
let ( &+ ) req1 req2 = 
  reqMerge req1 req2

let rec ccTypecheck tm =
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
    let (tm'Typ, tm'Req, tm'Cst) = ccTypecheck tm' in
    let bndCst = (%*) (bndTyp, tm'Req #< bind) in
    let outCst = tm'Cst %+ bndCst in
    let outReq = tm'Req /< bind in
    (Arrow (bndTyp, tm'Typ), outReq, outCst)
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
  | TFix (tm, typ) -> 
    let (_, tmReq, tmCst) = ccTypecheck tm in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let newCst = (%*) (typ, Arrow (freshTyp, freshTyp)) in
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
