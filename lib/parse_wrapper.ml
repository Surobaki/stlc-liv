open Errors
open Stlc_lexrules
open Lexing

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
