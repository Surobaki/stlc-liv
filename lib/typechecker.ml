open Stlc
open Typechecker_aux
    
let _APP_ARG_ERR = Errors.Type_error ("Failed to match argument of function with function signature.")
let _NONFUNC_APP_ERR = Errors.Type_error ("Non-functional application.") 
let _OP_ARG_ERR = Errors.Type_error ("Failed to match argument of operator with expected type.")
let _CTXT_VAR_NOT_FOUND_ERR = Errors.Type_error ("Failed to match provided variable with a variable in the context.")
let _FIX_ARG_ERR = Errors.Type_error ("Failed to check the type of a fixed point construction.")
let _IF_ARG_ERR = Errors.Type_error ("Failed to check the type of an if branch.")

let rec robTypecheck env tm = 
  match tm with
  | TConstant (CInteger _) -> Integer
  | TConstant (CBoolean _) -> Boolean
  | TVariable var -> 
    (match (List.assoc_opt var env) with
     | Some typ -> typ
     | None -> raise _CTXT_VAR_NOT_FOUND_ERR)
  | TAbstract (bind, typ, tm') -> 
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
    if tm1Typ = Boolean && tm2Typ = tm3Typ then tm3Typ
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
       | Integer, Integer -> Integer
       | _ -> raise _OP_ARG_ERR)
     | Lt | Le | Gt | Ge ->
       (match tm1Typ, tm2Typ with
       | Integer, Integer -> Boolean
       | _ -> raise _OP_ARG_ERR)
     | Eq | Neq ->
       if tm1Typ = tm2Typ then Boolean
       else raise _OP_ARG_ERR)
  | TLet (bind, bndTm, coreTm) ->
    let bndTmTyp = robTypecheck env bndTm in
    let enrichedEnv = ((bind, bndTmTyp) :: env) in
    robTypecheck enrichedEnv coreTm
    
                   
