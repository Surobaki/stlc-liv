open Stlc
open Typechecker_aux
    
let _APP_ARG_ERR = Errors.Type_error 
  ("Failed to match argument of function with function signature.")
let _NONFUNC_APP_ERR = Errors.Type_error 
  ("Non-functional application.") 
let _OP_ARG_ERR = Errors.Type_error 
  ("Failed to match argument of operator with expected type.")
let _CTXT_VAR_NOT_FOUND_ERR = Errors.Type_error 
  ("Failed to match provided variable with a variable in the context.")
let _FIX_ARG_ERR = Errors.Type_error 
  ("Failed to check the type of a fixed point construction.")
let _IF_ARG_ERR = Errors.Type_error 
  ("Failed to check the type of an if branch.")
  
let contextLookup (var : livBinder) (context : robEnv) : livTyp =
  match (List.assoc_opt var context) with
  | Some typ -> typ
  | None -> raise _CTXT_VAR_NOT_FOUND_ERR

let rec robTypecheck (env : robEnv) (tm : livTerm) : livTyp = 
  match tm with
  | TConstant (CInteger _) -> Base Integer
  | TConstant (CBoolean _) -> Base Boolean
  | TVariable var -> contextLookup var env
  | TAbstract (bind, typ, tm') -> 
    let enrichedEnv = ((bind, typ) :: env) in 
    let tm'Typ = robTypecheck enrichedEnv tm' in
      Arrow (typ, tm'Typ)
  (* TODO: Implement linearity *)
  | TLinAbstract (bind, typ, tm') ->
    let enrichedEnv = ((bind, typ) :: env) in 
    let tm'Typ = robTypecheck enrichedEnv tm' in
      Arrow (typ, tm'Typ)
  | TApplication (tm1, tm2) -> 
    let tm1Typ = robTypecheck env tm1 in
    let tm2Typ = robTypecheck env tm2 in
    let (firstArg, sndArg) = unpackArrow tm1Typ in
              if firstArg = tm2Typ then sndArg
              else raise _APP_ARG_ERR
  | TIf (tm1, tm2, tm3) ->
    let tm1Typ = robTypecheck env tm1 in
    let tm2Typ = robTypecheck env tm2 in
    let tm3Typ = robTypecheck env tm3 in
    if tm1Typ = (Base Boolean) && tm2Typ = tm3Typ then tm3Typ
    else raise _IF_ARG_ERR
  | TFix (tm', typ) ->
    let tm'Typ = robTypecheck env tm' in
    let (_, t2) = unpackArrow tm'Typ in
    if t2 = typ then t2
    else raise _FIX_ARG_ERR
  | TBinOp (op, tm1, tm2) ->
    let tm1Typ = robTypecheck env tm1 in
    let tm2Typ = robTypecheck env tm2 in
    (match op with
     | Plus | Minus | Mult | Div ->
       (match tm1Typ, tm2Typ with
       | Base Integer, Base Integer -> Base Integer
       | _ -> raise _OP_ARG_ERR)
     | Lt | Le | Gt | Ge ->
       (match tm1Typ, tm2Typ with
       | Base Integer, Base Integer -> Base Boolean
       | _ -> raise _OP_ARG_ERR)
     | Eq | Neq ->
       if tm1Typ = tm2Typ then Base Boolean
       else raise _OP_ARG_ERR)
  | TLet (bind, bndTm, coreTm) ->
    let bndTmTyp = robTypecheck env bndTm in
    let enrichedEnv = ((bind, bndTmTyp) :: env) in
    robTypecheck enrichedEnv coreTm
                   
