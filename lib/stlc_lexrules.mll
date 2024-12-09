(* 
  Author: Olivia Hartley Weston
  Boilerplate plundered from:
  https://github.com/SimonJF/mbcheck/blob/main/lib/frontend/lexer.mll
*)
{
open Stlc_parser
open Lexing

(* Increments internal lexer metadata *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

(* Regex macros *)
let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']*)
let def_white = [' ' '\t']+
let def_newline = '\r' | '\n' | "\r\n"
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
(* let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?) *)

rule tokenise = parse
  (* Whitespace, newlines *)
  | def_white { tokenise lexbuf }
  | def_newline { next_line lexbuf; tokenise lexbuf }
  (* Keywords *)
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "match" { MATCH }
  | "with" { WITH }
  | "fork" { FORK }
  | "wait" { WAIT }
  | "inl" { INL }
  | "inr" { INR }
  | "send" { SEND }
  | "receive" { RECEIVE }
  | "endbang" { ENDBANG }
  | "endquery" { ENDQUERY }
  | "Int" { TYINT }
  | "Bool" { TYBOOL }
  | "Unit" { UNIT }
  | "fix" { FIX }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  (* Multi-character symbols *)
  | "->" { ARROW }
  | "-@" { LOLLI }
  | ">=" { GE }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  (* Single-character symbols *)
  | '@' { AT }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { FSLASH }
  | '<' { LANGLE }
  | '>' { RANGLE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '\\' { LAMBDA }
  | '=' { EQUALS }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | '.' { DOT }
  | '!' { BANG }
  | '?' { QSTNMARK }
  | '&' { AMPERSAND }
  | eof { EOF }
  (* Variables and integer literals *)
  | def_id as var { VARIABLE var }
  | def_integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
