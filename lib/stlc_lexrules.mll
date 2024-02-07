{
  open stlc_parser

  (*
     Plundered from
     https://github.com/SimonJF/mbcheck/blob/main/lib/frontend/lexer.mll
  *)
  let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']*)
  let def_white = [' ' '\t']+
  let def_newline = '\r' | '\n' | "\r\n"
  let def_integer = (['1'-'9'] ['0'-'9']* | '0')
  let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)
}

rule tokenise = parse
  | def_white
    { tokenise lexbuf }
  | def_newline
    { next_line lexbuf; tokenise lexbuf }
  | def_Integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" | "false" | "True" | "False" | "TRUE" | "FALSE"
    { BOOL }
  | "->"
    { ARROW }
  | def_id as var 
    { VAR }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '\\'
    { LAMBDA }
  | ':'
    { COLON }
  | eof
    { LexEOF }