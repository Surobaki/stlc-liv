open Stlc

let _NONFUNC_APP_ERR = Errors.Type_error ("Non-functional application.") 

let unpackArrow e = match e with Arrow (e1, e2) -> (e1, e2)
                                 | _ -> raise _NONFUNC_APP_ERR
