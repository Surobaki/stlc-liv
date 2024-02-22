let _APP_ARG_ERR = Errors.Type_error ("Failed to match argument of function with function signature.")
let _NONFUNC_APP_ERR = Errors.Type_error ("Non-functional application.") 
let _OP_ARG_ERR = Errors.Type_error ("Failed to match argument of operator with expected type.")

let rec typecheck env tm = 
  match tm with
  | TConstant con -> match con with
                     | CInteger _ -> Integer
                     | CBoolean _ -> Boolean
  | TVariable var -> 
    snd (List.find 
          (fun elem -> match elem with
          | (srchVar, _) -> var == srchVar
          | _ -> false) 
        env)
  | TAbstract (bind, typ, tm') -> 
    let enrichedEnv = (cons (bind, typ) env) in 
    let tm'Typ = typecheck enrichedEnv tm' in
      Arrow (typ, tm'Typ)
  | TApplication (tm1, tm2) -> 
      let tm1Typ = typecheck tm1 in
      let tm2Typ = typecheck tm2 in
        match tm1Typ with
        | Arrow (firstArg, _) -> 
               if firstArg == tm2Typ then
               Arrow (tm1Typ, tm2Typ)
               else
               raise APP_ARG_ERR
        | _ -> raise NONFUNC_APP_ERR
  | TBinOp (op, tm1, tm2) ->
    match op with
    | Plus | Minus | Mult | Div ->
      let tm1Typ = typecheck tm1 in
      let tm2Typ = typecheck tm2 in
        match tm1Typ, tm2Typ with
        | Integer, Integer -> Integer
        | _ -> raise _OP_ARG_ERR
    | Lt | Le | Gt | Ge ->
      let tm1Typ = typecheck tm1 in
      let tm2Typ = typecheck tm2 in
        match tm1Typ, tm2Typ with
        | Integer, Integer -> Boolean
        | _ -> raise _OP_ARG_ERR
    | Eq | Neq ->
      let tm1Typ = typecheck tm1 in
      let tm2Typ = typecheck tm2 in
        if tm1Typ == tm2Typ then Boolean
                            else raise _OP_ARG_ERR
                   
