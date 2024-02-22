open STLC.Parse_wrapper
open STLC.Stlc

let rec rppl () = 
    let raw_content = read_line () in
    let term = parse_string raw_content in
    print_endline (show_livTerm term);
    rppl ()
  
let () = rppl ()
