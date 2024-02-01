{
  type livToken = LexNumber
		| LexBool | LexVar | LexAdd
		| LexSub | LexMult | LexDiv
		| LexLt | LexLe | LexEq
		| LexNeq | LexGt | LexGe
		| LexIfBool | LexIfInt | LexLBracket
		| LexRBracket | LexLambda | LexBinderSeparator
		| LexArrow | LexEOF ;;
}

rule tokenise = parse
  [' ' '\t' '\n']
    { tokenise lexbuf }
  | ['0'-'9']+
    { LexNumber }
  | "true" | "false" | "True" | "False" | "TRUE" | "FALSE"
    { LexBool }
  | "->"
    { LexArrow }
  | '+'
    { LexAdd }
  | '-'
    { LexSub }
  | '*'
    { LexMult }
  | '/'
    { LexDiv }
  | '='
    { LexEq }
  | "!="
    { LexNeq }
  | "<="
    { LexLe }
  | '<'
    { LexLt }
  | ">="
    { LexGe }
  | '>'
    { LexGt }
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
  | '\\'
    { LexLambda }
  | ':'
    { LexBinderSeparator }
  | eof
    { LexEOF }