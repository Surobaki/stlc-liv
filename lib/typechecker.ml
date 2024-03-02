open Stlc

let _APP_ARG_ERR = Errors.Type_error ("Failed to match argument of function with function signature.")
let _NONFUNC_APP_ERR = Errors.Type_error ("Non-functional application.") 
let _OP_ARG_ERR = Errors.Type_error ("Failed to match argument of operator with expected type.")
let _CTXT_VAR_NOT_FOUND_ERR = Errors.Type_error ("Failed to match provided variable with a variable in the context.")

let rec typecheck env tm = 
  match tm with
  | TConstant con -> (match con with
                      | CInteger _ -> Integer
                      | CBoolean _ -> Boolean)
  | TVariable var -> 
    (match (List.assoc_opt var env) with
     | Some typ -> typ
     | None -> (Errors.print_raise_exn _CTXT_VAR_NOT_FOUND_ERR))
  | TAbstract (bind, typ, tm') -> 
    let enrichedEnv = (List.cons (bind, typ) env) in 
    let tm'Typ = typecheck enrichedEnv tm' in
      Arrow (typ, tm'Typ)
  | TApplication (tm1, tm2) -> 
      let tm1Typ = typecheck env tm1 in
      let tm2Typ = typecheck env tm2 in
        (match tm1Typ with
         | Arrow (firstArg, sndArg) -> 
                if firstArg = tm2Typ then
                  sndArg
                else
                (Errors.print_raise_exn _APP_ARG_ERR)
         | _ -> (Errors.print_raise_exn _NONFUNC_APP_ERR))
  | TBinOp (op, tm1, tm2) ->
    (match op with
     | Plus | Minus | Mult | Div ->
       let tm1Typ = typecheck env tm1 in
       let tm2Typ = typecheck env tm2 in
         (match tm1Typ, tm2Typ with
          | Integer, Integer -> Integer
          | _ -> (Errors.print_raise_exn _OP_ARG_ERR))
     | Lt | Le | Gt | Ge ->
       let tm1Typ = typecheck env tm1 in
       let tm2Typ = typecheck env tm2 in
         (match tm1Typ, tm2Typ with
          | Integer, Integer -> Boolean
          | _ -> (Errors.print_raise_exn _OP_ARG_ERR))
     | Eq | Neq ->
       let tm1Typ = typecheck env tm1 in
       let tm2Typ = typecheck env tm2 in
         if tm1Typ == tm2Typ then Boolean
                             else (Errors.print_raise_exn _OP_ARG_ERR))
                   
