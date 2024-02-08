{
open Stlc_parser
open Lexing

(*
   Plundered from
   https://github.com/SimonJF/mbcheck/blob/main/lib/frontend/lexer.mll
*)

(* Increments internal lexer metadata *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']*)
let def_white = [' ' '\t']+
let def_newline = '\r' | '\n' | "\r\n"
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)

rule tokenise = parse
  | def_white
    { tokenise lexbuf }
  | def_newline
    { next_line lexbuf; tokenise lexbuf }
  | def_integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true"
    { BOOL true }
  | "false"
    { BOOL false }
  | "->"
    { ARROW }
  | "Int"
    { TYINT }
  | "Bool"
    { TYBOOL }
  | '+'
    { PLUS }
  | '-'
    { MINUS }
  | '*'
    { STAR }
  | '/'
    { FSLASH }
  | ">="
    { GE }
  | '>'
    { GT }
  | "=="
    { EQ }
  | "!="
    { NEQ }
  | "<="
    { LE }
  | '<'
    { LT }
  | def_id as var 
    { VARIABLE var }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '\\'
    { LAMBDA }
  | ':'
    { COLON }
  | '.'
    { DOT }
  | eof
    { EOF }