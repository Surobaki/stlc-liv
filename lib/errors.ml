exception Parse_error of string
exception Syntax_error of string
exception Type_error of string
    
let print_raise_exn excep = print_endline (Printexc.to_string excep);
                            raise excep
