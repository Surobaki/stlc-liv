open Stlc
    
module StringSet = Set.Make (String)
    
let unpackTyp triple = match triple with (typ, _, _) -> typ

let unpackReq triple = match triple with (_, req, _) -> req

let unpackCst triple = match triple with (_, _, cst) -> cst

let reqDom req =
  let reqList = TypeRequirements.to_list req in
  let reqBindList = List.map (fun x -> match x with (bnd, _) -> bnd) reqList in
  StringSet.of_list reqBindList

let reqMerge req1 req2 =
  let req1Dom = reqDom req1 in
  let req2Dom = reqDom req2 in
  let req1List = TypeRequirements.to_list req1 in
  let req2List = TypeRequirements.to_list req2 in
  let reqDomComp = StringSet.diff req1Dom req2Dom in
  let reqDomCompList = StringSet.to_list reqDomComp in
  let cstDomInter = StringSet.inter req1Dom req2Dom in
  let cstDomInterList = StringSet.to_list cstDomInter in
  let newReqList = List.map (fun x -> (x, (List.assoc x req2List))) 
                            reqDomCompList in
  let newReqSet = TypeRequirements.of_list newReqList in
  let outReq = TypeRequirements.union req1 newReqSet in
  let outCstList = List.map (fun x -> 
                        (List.assoc x req1List, List.assoc x req2List)) 
                        cstDomInterList in
  let outCst = TypeConstraints.of_list outCstList in
  (outReq, outCst)

let reqMerge3 req1 req2 req3 =
  let (req12, cst12) = reqMerge req1 req2 in
  let (req123, cst23) = reqMerge req12 req3 in
  let cst123 = TypeConstraints.union cst12 cst23 in
  (req123, cst123)

let rec ccTypecheck tm =
  match tm with
  | TConstant (CInteger _) -> 
      (Integer, TypeRequirements.empty, TypeConstraints.empty)
  | TConstant (CBoolean _) -> 
      (Boolean, TypeRequirements.empty, TypeConstraints.empty)
  | TVariable var -> 
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let freshBind = (var, freshTyp) in
    (freshTyp, TypeRequirements.singleton freshBind, TypeConstraints.empty)
  | TAbstract (bind, bndTyp, tm') ->
    let (tm'Typ, tm'Req, tm'Cst) = ccTypecheck tm' in
    let tm'ReqList = TypeRequirements.to_list tm'Req in
    let bndCst = (match List.assoc_opt bind tm'ReqList with
                 | Some typ -> TypeConstraints.singleton (bndTyp, typ)
                 | None -> TypeConstraints.empty) in
    let outCst = TypeConstraints.union tm'Cst bndCst in
    let outReq = TypeRequirements.remove (bind, bndTyp) tm'Req in
    (Arrow (bndTyp, tm'Typ), outReq, outCst)
  | TApplication (tm1, tm2) ->
    let (tm1Typ, tm1Req, tm1Cst) = ccTypecheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = ccTypecheck tm2 in
    let (req12, cst12) = reqMerge tm1Req tm2Req in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let appCst = TypeConstraints.singleton (tm1Typ, Arrow (tm2Typ, freshTyp)) in
    let outCst = TypeConstraints.union (TypeConstraints.union tm1Cst tm2Cst)
                                       (TypeConstraints.union appCst cst12) in
    (freshTyp, req12, outCst)
  | TLet (_, _, _) -> (Integer, TypeRequirements.empty, TypeConstraints.empty)
  | TIf (tm1, tm2, tm3) ->
    let (tm1Typ, tm1Req, tm1Cst) = ccTypecheck tm1 in
    let (tm2Typ, tm2Req, tm2Cst) = ccTypecheck tm2 in
    let (tm3Typ, tm3Req, tm3Cst) = ccTypecheck tm3 in
    let (req123, cst123) = reqMerge3 tm1Req tm2Req tm3Req in
    let predicateCst = TypeConstraints.singleton (tm1Typ, Boolean) in
    let homoResultCst = TypeConstraints.singleton (tm2Typ, tm3Typ) in
    let ifCst = TypeConstraints.union predicateCst homoResultCst in
    let inputCst = TypeConstraints.union (TypeConstraints.union tm1Cst tm2Cst)
                                         tm3Cst in
    let outCst = TypeConstraints.union (TypeConstraints.union ifCst inputCst)
                                       cst123 in
    (tm2Typ, req123, outCst)
  | TFix (tm, typ) -> 
    let (_, tmReq, tmCst) = ccTypecheck tm in
    let freshTyp = TypeVar (TyVar.fresh ()) in
    let newCst = TypeConstraints.singleton 
                   (typ, Arrow (freshTyp, freshTyp)) in
    let outCst = TypeConstraints.union newCst tmCst in
    (freshTyp, tmReq, outCst)
  | TBinOp (_, _, _) -> (Integer, TypeRequirements.empty, TypeConstraints.empty)

