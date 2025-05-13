£ := end of file literal
i, b, v := metavariables representing (i)nteger, (b)oolean and (v)ariables
x, y, x1, x2 := binders

Term ::= Expr £

STypNary ::= STyp | STyp, STypNary

STyp ::=   endbang | endquery
         | !Typ.STyp | ?Typ.STyp
         | (+) STypNary | (&) STypNary

TypKeyword ::= 'Integer' | 'Boolean'
Typ ::=   <STyp>
        | 'unit' | (Typ)
        | Typ -> Typ | Typ -@ Typ
        | Typ + Typ | Typ * Typ
        | TypKeyword

Fact ::= i | b | v | (Expr)
Operator ::= '<' | '<=' | '>' | '>=' | '==' | '!=' | '+' | '-' | '*' | '/'
Operation ::=   Operation Operator Operation
              | Fact

Expr ::=   \x : Typ . Expr
         | \x :@ Typ . Expr
         | Fact Expr
         | Operation
         | let (x1, x2) = Expr in Expr
         | inl Expr | inr Expr
         | match Expr with (x1 -> Expr , x2 -> Expr)
         | Expr ; Expr
         | let x = Expr in Expr
         | fix Expr : Typ
         | if Expr then Expr else Expr
         | (Expr, Expr)
         | send Expr Expr | receive Expr
         | fork Expr | wait Expr
