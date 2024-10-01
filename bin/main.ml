open STLC.Parse_wrapper
open STLC.Stlc
open STLC.Cctx_typechecker

let rec rppl () = 
    print_endline "Welcome to the Read-Parse-Print Loop";
    let raw_content = read_line () in
    let term = parse_string raw_content in
    print_endline (show_livTerm term);
    rppl ()
      
let rec rptpl_lin () =
    let raw_content = read_line () in
    let termTyp = ccTypecheck_string 
                  raw_content linBrMerge linSeqMerge linCheck in
    print_endline (show_livTyp (fst termTyp));
    print_endline "Type checking finished.";
    rptpl_lin ()

let rec rptpl_mix () =
    let raw_content = read_line () in
    let termTyp = ccTypecheck_string 
                  raw_content mixBrMerge mixSeqMerge mixCheck in
    print_endline (show_livTyp (fst termTyp));
    print_endline "Type checking finished.";
    rptpl_mix ()

let rec rptpl_unr () =
    let raw_content = read_line () in
    let termTyp = ccTypecheck_string 
                  raw_content unrMerge unrMerge unrCheck in
    print_endline (show_livTyp (fst termTyp));
    print_endline "Type checking finished.";
    rptpl_unr ()
      
let rec rptpl () =
    print_endline "Welcome to the Read-Parse-Typecheck-Print Loop";
    print_endline "Which linearity base would you like to use?";
    print_endline "(1) Linear | (2) Mixed | (3) Unrestricted";
    let choice = read_line () in
    match choice with
    | "1" -> rptpl_lin ()
    | "2" -> rptpl_mix ()
    | "3" -> rptpl_unr ()
    | _ -> rptpl ()
    
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
