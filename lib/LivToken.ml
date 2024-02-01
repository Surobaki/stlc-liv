(* File containing token declaration for lexer *)
type livToken = LexNumber
              | LexBool
              | LexVar
              | LexAdd
              | LexSub
              | LexMult
              | LexDiv
              | LexLt
              | LexLe
              | LexEq
              | LexNeq
              | LexGt
              | LexGe
              | LexIfBool
              | LexIfInt
              | LexLBracket
              | LexRBracket
              | LexLambda
              | LexBinderSeparator
              | LexArrow
              | LexEOF;;
