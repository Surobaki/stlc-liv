{
  open LivToken
}

rule token = parse
  [' ' '\t' '\n']
    { token lexbuf }
  | ['0'-'9']+
    { LexNumber }
  | "true" | "false" | "True" | "False" | "TRUE" | "FALSE"
    { LexBool }
  | '+'
    { LexAdd }
  | '-'
    { LexSub }
  | '*'
    { LexMult }
  | '/'
    { LexDiv }
  | '<'
    { LexLt }
  | '='
    { LexEq }
  | "<="
    { LexLe }
  | "!="
    { LexNeq }
  | '>'
    { LexGt }
  | ">="
    { LexGe }
  | "ifB"
    { LexIfBool }
  | "ifI"
    { LexIfInt }
  | ['a'-'z''A'-'Z']+
    { LexVar }
  | '('
    { LexLBracket }
  | ')'
    { LexRBracket }
  | eof
    { LexEOF }