open Stlc
open Interpreter_aux
    
let _ENV_VAR_NOT_FOUND_ERR = Errors.Runtime_error ("Failed to find bound value for variable identifier.")
let _NONFUNC_APP_ERR = Errors.Runtime_error ("Non-functional application.")

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
  | TBinOp (op, tm1, tm2) ->
    let tm1Val = gremEval env tm1 in
    let tm2Val = gremEval env tm2 in
    (match op with
     | Plus -> tm1Val + tm2Val
     | Minus -> tm1Val - tm2Val
     | Mult -> tm1Val +* tm2Val
     | Div -> tm1Val / tm2Val
     | Lt -> tm1Val < tm2Val
     | Le -> tm1Val <= tm2Val
     | Gt -> tm1Val > tm2Val
     | Ge -> tm1Val >= tm2Val
     | Eq -> tm1Val = tm2Val
     | Neq -> tm1Val <> tm2Val)
    
