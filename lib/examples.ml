open Parse_wrapper

let ex1 = parse_string "(\\x:Int.x)5" (* Should always pass *)
let ex2 = parse_string "((\\x:Int.(\\y:Int.x+y))2)3" (* Should always pass *)
let ex3 = parse_string "(\\x:Int.(\\y:Bool.x))4" (* Fails with pure linearity,
                                            generates constraint with mixed *)
let ex4 = parse_string "(((\\x:Bool.(\\y:Int.(\\z:Int.if x then y+y else y)))true)6)8"

let ex6 = parse_string {|
let a = 5 + 5 in
a * a
|}

let ex7 = parse_string {|
let (a,b) = (5,5) in
a
|}

    (* Same thing, fails with pure linearity. *)
let ex5 = parse_string 
{|
let coolSeshFun = (\channel_tx :@ <?Int.!Int.?Int.endbang> .
  let (hundo, channel_tx) = receive channel_tx in 
  let channel_tx = send (hundo * 5) channel_tx in 
  let (digglebyPlusOne, channel_tx) = receive channel_tx in 
  channel_tx) 
in fork coolSeshFun|}

let ex5_1 = parse_string {|
let coolSeshFun = (\channel_tx :@ <?Int.!Int.?Int.endbang> .
  let (hundo, channel_tx1) = receive channel_tx in 
  let channel_tx2 = send (hundo * 5) channel_tx1 in 
  let (digglebyPlusOne, channel_tx3) = receive channel_tx2 in 
  channel_tx3)
in fork coolSeshFun
|}

let ex5_2 = parse_string {|
\c : <!Int.endbang> . 
let a = send 1 c in
a
|}
