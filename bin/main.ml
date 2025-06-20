open CoreLang.Errors
open CoreLang.Cctx_typechecker
open CoreLang.Parse_wrapper
open Out_channel
       
let _ERR_NO_FILE = Runtime_error "Missing input files."
let _ERR_UNREC_BASE = Runtime_error {|Unrecognised linearity base. 
                                      Try one of the following: 
                                      lin ; mix ; unr"|}

(* Argument parsing specification *)
let usage_message = "tc [-base <linearitybase>] [-verbose] <inputfile1> [<inputfile2 [...]] [-o <outputfile>]"

let arg_linearity_base = ref ""
let arg_verbose = ref false
let arg_input_file = ref []
let arg_output_file = ref ""
    
let anon_fun input = arg_input_file := input :: !arg_input_file
    
let args_specification = 
  [("-base", 
    Arg.Set_string arg_linearity_base, 
    "Set linearity base: lin[ear] | mix[ed] | unr[estricted].");
   ("-verbose", 
    Arg.Set arg_verbose, 
    "Increase verbosity level.");
   ("-o",
    Arg.Set_string arg_output_file,
    "Set output file.")]

(* Rudimentary data validation *)
let secure_base (b : string) : linearityBase =
  match b with
  | "l" | "lin" | "linear" -> B_Linear
  | "m" | "mix" | "mixed" -> B_Mixed
  | "u" | "unr" | "unrestricted" -> B_Unrestricted
  | _ -> raise _ERR_UNREC_BASE

let secure_filepath (p : string) : string =
  if Sys.file_exists p 
  then p
  else (Format.eprintf "Error: cannot find path %s" p; "")

let _secure_filepaths (ps : string list) : string list =
  List.map secure_filepath ps

type unsafe_arguments = {
  lin_base_str : string;
  verbosity : bool;
  out_file : string;
  in_files : string list
}

type safe_arguments = {
  lin_base : linearityBase;
  verbosity : bool;
  out_file : string;
  in_files : string list
}

let secure_argument { lin_base_str = lb; verbosity = v; 
                       out_file = _o; in_files = i } : safe_arguments =
  { lin_base = secure_base lb; verbosity = v;
    out_file = ""; (* secure_filepath o; *)
    in_files = [List.hd i];
    (* in_files = List.filter (fun fp -> fp <> "" && fp <> "\n" && fp <> "\r") (secure_filepaths i) *) }

(* Wrapper for type checking *)
let typecheck_wrapper { lin_base = lb; verbosity = v; 
                        out_file = o; in_files = i } : unit =
  if i = [] then exit 2 else
  let parsed_files = List.map parse_file i in
  (* AST Debug Printing *)
  List.iter 
    (fun x -> 
      Format.printf "@[The AST:@;<1 2>%a@]@." 
      CoreLang.Ast.pp_term x) 
    parsed_files;
  let checked_files = List.map (bobTypecheck lb) parsed_files in
  let final_string = Format.(
                     asprintf "@[Typecheck results: @;<1 2>@[%a@]@" 
                     (pp_print_list ~pp_sep:(fun ppf () -> 
                                             Format.fprintf ppf "@.") 
                       (pp_tcOut ~verbose:v))
                     checked_files) in
  match o with
  | "" -> print_string final_string; exit 0
  | path -> 
    let channel = open_gen [Open_wronly; Open_creat] 0o664 path in
    output_string channel final_string; close channel; exit 0
  
(* Entry point *)
let () = Arg.parse args_specification anon_fun usage_message;
         let unsafe_args = { lin_base_str = !arg_linearity_base; 
                             verbosity = !arg_verbose; 
                             out_file = !arg_output_file; 
                             in_files = !arg_input_file } in
         let args = unsafe_args.in_files in
         Format.printf "@[<hov 2>Arg count: %d,@ args: [%a]@]@;<1 2>" 
           (List.length args)
           (Format.pp_print_list Format.pp_print_string) args;

         let safe_args = secure_argument unsafe_args in
         typecheck_wrapper safe_args
