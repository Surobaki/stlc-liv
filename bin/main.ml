open STLC.Parse_wrapper
open STLC.Stlc

let rec rppl () = 
    print_endline "Welcome to the Read-Parse-Print Loop";
    let raw_content = read_line () in
    let term = parse_string raw_content in
    print_endline (show_livTerm term);
    rppl ()
      
let rec rptpl () =
    print_endline "Welcome to the Read-Parse-Typecheck-Print Loop";
    let raw_content = read_line () in
    let termTyp = typecheck_string [] raw_content in
    print_endline (show_livTyp termTyp);
    rptpl ()

let rec repl () =
    print_endline "Welcome to the Read-Evaluate-Print Loop";
    let raw_content = read_line () in
    full_treatment_string raw_content;
    repl ()

let rec initialise () =
  print_endline "Hi, please select your desired operation:";
  print_endline "(1) RPPL | (2) RPTPL | (3) REPL";
  let choice = read_line () in
  match choice with
  | "1" -> rppl ()
  | "2" -> rptpl ()
  | "3" -> repl ()
  | _ -> initialise ()
  
let () = initialise ()
