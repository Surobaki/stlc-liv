open Errors
open Stlc
open Stlc_lexrules
open Lexing
open Typechecker
open Interpreter
open Cctx_typechecker

let print_position ppf lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf ppf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Stlc_parser.expr_main tokenise lexbuf with
  | Syntax_error msg ->
    let msg = Format.asprintf "%a: %s" print_position lexbuf msg in
    raise (Errors.Parse_error msg)
  | Stlc_parser.Error ->
    let msg = Format.asprintf "%a: syntax error" print_position lexbuf in
    raise (Parse_error msg)

let parse_file filename =
  let inx = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let expr = parse_with_error lexbuf in
  In_channel.close inx;
  expr

let parse_string x =
  let lexbuf = Lexing.from_string x in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "<string>" };
  let expr = parse_with_error lexbuf in
  expr
    
let typecheck_file env filename =
  let ast = parse_file filename in
  robTypecheck env ast
    
let typecheck_string env x =
  let ast = parse_string x in
  robTypecheck env ast
    
let ccTypecheck_string_simpl x =
  let ast = parse_string x in
  bobTypecheckSimple ast
    
let ccTypecheck_file filename =
  let ast = parse_file filename in
  bobTypecheckSimple ast
    
let ccTypecheck_string x br seq chk =
  let ast = parse_string x in
  bobTypecheck br seq chk ast
    
let eval_file env filename =
  let ast = parse_file filename in
  gremEval env ast

let eval_string env x =
  let ast = parse_string x in
  gremEval env ast

(* The full treatment is when you go to a salon
   and get your hair parsed, typechecked and evaluated. *)
let full_treatment_file ?(typEnv = []) ?(evalEnv = []) filename =
  let ast = parse_file filename in
  let resultType = typecheck_file typEnv filename in
  let ccType = ccTypecheck_file filename in
  Printf.printf "Typecheck: %s\n" (show_livTyp resultType); 
  Printf.printf "CCTX Typecheck: %s\n" (show_livTyp ccType);
  Printf.printf "Evaluates to: %s\n" (show_gremVal (gremEval evalEnv ast))

let full_treatment_string ?(typEnv = []) ?(evalEnv = []) x =
  let ast = parse_string x in
  let resultType = typecheck_string typEnv x in
  let ccType = ccTypecheck_string_simpl x in
  Printf.printf "Typecheck: %s\n" (show_livTyp resultType);
  Printf.printf "CCTX Typecheck: %s\n" (show_livTyp ccType);
  Printf.printf "Evaluates to: %s\n" (show_gremVal (gremEval evalEnv ast))
