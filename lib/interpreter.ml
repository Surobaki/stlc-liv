open Stlc
open Interpreter_aux
    
let _ENV_VAR_NOT_FOUND_ERR = Errors.Runtime_error ("Failed to find bound value for variable identifier.")
let _NONFUNC_APP_ERR = Errors.Runtime_error ("Non-functional application.")

let binEval op v1 v2 =
  match op with
  | Plus -> v1 + v2
  | Minus -> v1 - v2
  | Mult -> v1 +* v2
  | Div -> v1 / v2
  | Lt -> v1 < v2
  | Le -> v1 <= v2
  | Gt -> v1 > v2
  | Ge -> v1 >= v2
  | Eq -> v1 = v2
  | Neq -> v1 <> v2

let rec gremEval env tm = 
  match tm with
  | TConstant (CInteger i) -> VInteger i
  | TConstant (CBoolean b) -> VBoolean b
  | TVariable vStr -> (
    match (List.assoc_opt vStr env) with
    | Some vVal -> vVal
    | None -> raise _ENV_VAR_NOT_FOUND_ERR)
  | TAbstract (bind, _, tm') ->
    VClosure (bind, tm', env)
  | TLinAbstract (bind, _, tm') ->
    VClosure (bind, tm', env)
  | TApplication (tm1, tm2) ->
    let tm1' = gremEval env tm1 in
    let tm2' = gremEval env tm2 in
    let (bind, tm1'', tm1'Env) = unpackClosure tm1' in
    let closedEnv = ((bind, tm2') :: tm1'Env) in
    gremEval closedEnv tm1''
  | TFix (tm', _) ->
    let tm'Val = gremEval env tm' in
    let (bnd, tm'', env') = unpackClosure tm'Val in
    let extendedEnv = ((bnd, VClosure (bnd, tm'', env')) :: env') in
    gremEval extendedEnv tm''
  | TLet (bind, bndTm, coreTm) ->
    let bndTmVal = gremEval env bndTm in
    let extendedEnv = ((bind, bndTmVal) :: env) in
    gremEval extendedEnv coreTm
  | TIf (tm1, tm2, tm3) ->
    let tm1Bool = unpackBoolean (gremEval env tm1) in
    if tm1Bool then gremEval env tm2
    else gremEval env tm3
  | TBinOp (op, tm1, tm2) ->
    let tm1Val = gremEval env tm1 in
    let tm2Val = gremEval env tm2 in
    binEval op tm1Val tm2Val
    
