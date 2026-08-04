open CoreLang.Errors
open CoreLang.Cctx_typechecker
open CoreLang.Parse_wrapper
open Out_channel
open Cmdliner
       
let _ERR_NO_FILE = Runtime_error "Missing input files."
let _ERR_UNREC_BASE = Runtime_error "Unrecognised linearity base. Try one of the following: 'lin ; mix ; unr'."

(* Rudimentary data validation *)
let secure_base (b : string) : linearityBase =
  let trim base = String.sub base 0 (String.length b) in
  if String.equal b (trim "linear") then B_Linear
  else if String.equal b (trim "mixed") then B_Mixed
  else if String.equal b (trim "unrestricted") then B_Unrestricted
  else if String.equal b (trim "affine") then B_Affine
  else if String.equal b (trim "relevant") then B_Relevant
  else raise _ERR_UNREC_BASE

let base_argument =
  let parser s = 
    try
      Ok (secure_base s)
    with _ERR_UNREC_BASE -> 
      Error "Could not parse substructural base. Try one of the following: [mix;lin;unr;aff;rel]."
  in
  Arg.Conv.make ~docv:"BASE" ~parser ~pp:pp_linearityBase ()
  
let lin_base =
  let doc = "Typecheck in $(docv) substructural mode. Accepts (a substring of) mixed, linear, unrestricted, affine, relevant." in
  Arg.(value & opt base_argument B_Mixed & info ["b"; "base"] ~doc ~docv:"BASE")

let output_file =
  let doc = "Write output to $(docv)." in
  Arg.(value & opt filepath "" & info ["o"; "outfile"] ~doc ~docv:"OUTFILE")

let input_files = 
  let doc = "Read input from $(docv)." in
  Arg.(value & pos_all filepath [] & info [] ~doc ~docv:"INFILE")

let maybe_check lb tm filename = try Either.Left (finalCheck lb tm) with 
  | (Type_error msg) -> 
    Either.Right (Format.asprintf "Failed typechecking %s. Reason: %s" filename msg)

(* Wrapper for type checking *)
let typecheck_wrapper ((lb, o, i) : (linearityBase * string * string list)) : int =
  if List.compare_length_with i 0 = 0 then (Format.eprintf "No inputs provided. Shutting down.@."; exit 2) else
  let parsed_files = List.map parse_file i in
  let maybe_checked = 
    List.fold_right2 
    (fun filename parsed out -> 
      (filename, maybe_check lb parsed filename) :: out) 
    i parsed_files [] in
  let final_string = 
    List.fold_right 
    (fun (filename, checked_file) out -> 
      match checked_file with 
      | Either.Left check_out -> String.cat (Format.asprintf "Typechecking results for %s:@.@[%a@]" filename pp_tcOut check_out) 
                                (String.cat "\n" out)
      | Either.Right err_msg -> String.cat err_msg (String.cat "\n" out)) 
    maybe_checked "" in
  match o with
  | "" -> print_string @@ String.trim final_string; 0
  | path -> 
    let channel = open_gen [Open_wronly; Open_creat] 0o664 path in
    output_string channel (String.trim final_string); close channel; 0

let typecheck_term = Term.(
  const typecheck_wrapper $ 
    (const (fun lb oof iif -> (lb, oof, iif)) $ lin_base $ output_file $ input_files)
  )

let typecheck_command = 
  Cmd.make (Cmd.info "Typecheck one or more terms.") typecheck_term

let main () = Cmd.eval' typecheck_command
let () = if !Sys.interactive then () else exit (main ())
