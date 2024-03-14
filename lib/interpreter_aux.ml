open Stlc

let _ARITH_ARG_ERR = Errors.Runtime_error ("Arithmetic expression is given invalid arguments.")
let _REL_ARG_ERR = Errors.Runtime_error ("Relational expression is given invalid arguments.")
let _UNPACK_ERR = Errors.Runtime_error ("Failed to unpack value.")

let (+) e1 e2 = match e1, e2 with 
              | VInteger i1, VInteger i2 -> VInteger (i1 + i2)
              | _, _ -> raise _ARITH_ARG_ERR
           
let (-) e1 e2 = match e1, e2 with 
              | VInteger i1, VInteger i2 -> VInteger (i1 - i2)
              | _, _ -> raise _ARITH_ARG_ERR

let (+*) e1 e2 = match e1, e2 with 
               | VInteger i1, VInteger i2 -> VInteger (i1 * i2)
               | _, _ -> raise _ARITH_ARG_ERR

let (/) e1 e2 = match e1, e2 with 
              | VInteger _, VInteger 0 -> raise _ARITH_ARG_ERR
              | VInteger i1, VInteger i2 -> VInteger (i1 / i2)
              | _, _ -> raise _ARITH_ARG_ERR
                          
let (<) e1 e2 = match e1, e2 with
              | VInteger i1, VInteger i2 -> VBoolean (i1 < i2)
              | _, _ -> raise _REL_ARG_ERR

let (<=) e1 e2 = match e1, e2 with
              | VInteger i1, VInteger i2 -> VBoolean (i1 <= i2)
              | _, _ -> raise _REL_ARG_ERR

let (>) e1 e2 = match e1, e2 with
              | VInteger i1, VInteger i2 -> VBoolean (i1 > i2)
              | _, _ -> raise _REL_ARG_ERR

let (>=) e1 e2 = match e1, e2 with
              | VInteger i1, VInteger i2 -> VBoolean (i1 >= i2)
              | _, _ -> raise _REL_ARG_ERR

let (=) e1 e2 = match e1, e2 with
              | VInteger i1, VInteger i2 -> VBoolean (i1 = i2)
              | _, _ -> raise _REL_ARG_ERR

let (<>) e1 e2 = match e1, e2 with
              | VInteger i1, VInteger i2 -> VBoolean (i1 <> i2)
              | _, _ -> raise _REL_ARG_ERR

let unpackInteger e = match e with VInteger i -> i 
                                   | _ -> raise _UNPACK_ERR
let unpackBoolean e = match e with VBoolean b -> b 
                                   | _ -> raise _UNPACK_ERR
let unpackClosure e = match e with VClosure (bind, tm, env) -> (bind, tm, env) 
                                   | _ -> raise _UNPACK_ERR
