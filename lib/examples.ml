open Parse_wrapper

let ex1 = parse_string "(\\x:Int.x)5" (* Should always pass *)
let ex2 = parse_string "((\\x:Int.(\\y:Int.x+y))2)3" (* Should always pass *)
let ex3 = parse_string "(\\x:Int.(\\y:Bool.x))4" (* Fails with pure linearity,
                                            generates constraint with mixed *)
let ex4 = parse_string "(((\\x:Bool.(\\y:Int.(\\z:Int.if x then y+y else y)))true)6)8"
    (* Same thing, fails with pure linearity. *)
