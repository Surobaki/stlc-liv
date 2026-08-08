open CoreLang.Errors
open CoreLang.Cctx_typechecker
open CoreLang.Parse_wrapper
open Out_channel
open Cmdliner
       
let _ERR_NO_FILE = Runtime_error "Missing input files."
let _ERR_UNREC_BASE = Runtime_error "Unrecognised linearity base. Try one of the following: 'lin ; mix ; unr'."

module BaseMap = Map.Make (struct
  type t = linearityBase
  let compare = Stdlib.compare
end)

module Box = PrintBox

let _ALL_TESTS = ["test/base-terms-1.txt"; "test/base-terms-2.txt"; "test/base-terms-3.txt"; 
                  "test/base-terms-4.txt"; "test/comm-violation.txt"; "test/shopper.txt"; 
                  "test/simple-sess.txt"; "test/tcp.txt"]
let _TEST_SUITE = 
  BaseMap.singleton B_Unrestricted _ALL_TESTS
  |> BaseMap.add B_Linear _ALL_TESTS
  |> BaseMap.add B_Mixed _ALL_TESTS
  |> BaseMap.add B_Affine _ALL_TESTS
  |> BaseMap.add B_Relevant _ALL_TESTS

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
  let writing_params = [Open_wronly; Open_creat; Open_text] in
  match o with
  | "" -> print_string @@ String.trim final_string; 0
  | path -> 
    let channel = open_gen writing_params 0o664 path in
    output_string channel (String.trim final_string); close channel; 0

let testsuite_box =
  let input_data = BaseMap.fold 
  (fun lb tests out -> 
    BaseMap.add 
    lb 
    (tests, List.map 
      (fun test -> 
        parse_file test 
        |> (fun parsed -> maybe_check lb parsed test)) 
      tests) 
    out) 
  _TEST_SUITE 
  BaseMap.empty in
  Box.(
    frame @@ grid ~bars:true 
    (transpose @@ Array.of_list @@ List.concat [
      [Array.of_list @@ List.concat [[text "Filename"]; List.map text _ALL_TESTS]; ];
      List.map 
        (fun lb -> 
          Array.of_list @@ List.concat 
          [
            [text @@ Format.asprintf "%a" pp_linearityBase lb]; 
            List.map
            (fun result_sum -> 
              match result_sum with 
              | Either.Left _ -> text @@ Format.asprintf "%s" "✅"
              | Either.Right _ -> text @@ "❎") 
          (snd (BaseMap.find lb input_data))])
        [B_Unrestricted; B_Linear; B_Mixed; B_Affine; B_Relevant]
  ]))

let testsuite_wrapper (o : string) =
  let out_str = PrintBox_text.to_string testsuite_box in
  match o with
  | "" -> Format.printf "%s" out_str; 0
  | path -> 
    let writing_params = [Open_wronly; Open_creat; Open_text] in
    let channel = open_gen writing_params 0o664 path in
    output_string channel (String.trim out_str); close channel; 0

let typecheck_term = Term.(
  const typecheck_wrapper $ 
    (const (fun lb oof iif -> (lb, oof, iif)) $ lin_base $ output_file $ input_files)
  )

let testsuite_term = Term.(
  const testsuite_wrapper $ output_file
)

let typecheck_command = 
  Cmd.make (Cmd.info "typecheck") typecheck_term

let testsuite_command = Cmd.make (Cmd.info "testsuite") testsuite_term

let main_command = 
  Cmd.group (Cmd.info "ntextual") [typecheck_command; testsuite_command]

let main () = Cmd.eval' main_command
let () = if !Sys.interactive then () else exit (main ())
