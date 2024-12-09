%{
  open Ast
  open List
%}
(* Tokens *)
(* Primitive variable tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> VARIABLE

(* Parentheses *)
%token LPAREN RPAREN

(* Type construction grammar *)
%token TYINT TYBOOL UNIT
%token ARROW LOLLI
%token BANG QSTNMARK AMPERSAND
%token COLON DOT
%token ENDBANG ENDQUERY
%token LANGLE RANGLE

(* Common language constructions *)
%token IF THEN ELSE
%token FIX
%token SEMICOLON
%token COMMA
%token EOF
(* New binding of terms *)
%token LAMBDA AT
%token LET EQUALS IN
(* Sum type construction and elimination *)
%token INL INR
%token MATCH WITH

(* Session typing constructions *)
%token SEND RECEIVE
%token FORK WAIT

(* Arithmetic and relational binary_operations, 
   as well as PLUS and STAR for types. 
   GT and LT come from RANGLE and LANGLE. *)
%token PLUS MINUS STAR FSLASH
%token GE LE EQ NEQ

%left SEMICOLON
%left STAR PLUS

%right ARROW LOLLI

(* Assigning OCaml types to nonterminals *)
%type <term> expr binary_operation fact
%type <binOp> operator
%type <typ> ty base_ty
%type <sessTyp> sess_ty
%type <sessTyp list> nary_sess_ty

(* Start parsing *)
%start <term> expr_main

%%

expr:
  (* Lam *)
  | LAMBDA v = VARIABLE COLON t = ty DOT e = expr
    { TAbstract (v, t, e) }
  (* LinLam *)
  | LAMBDA v = VARIABLE COLON AT t = ty DOT e = expr
    { TLinAbstract (v, t, e) }
  (* App *)
  | e1 = fact e2 = expr { TApplication (e1, e2) }
  (* Binary binary_operations *)
  | o = binary_operation { o }
  (* Let product elimination *)
  | LET LPAREN v1 = VARIABLE COMMA v2 = VARIABLE RPAREN 
    EQUALS prodTm = expr IN continTm = expr
    { TLetProduct (v1, v2, prodTm, continTm) }
  (* Sum injections *)
  | INL e = expr { TInL e }
  | INR e = expr { TInR e }
  (* Sum elimination *)
  | MATCH scrutineeTm = expr WITH 
    LPAREN bindLeft = VARIABLE ARROW eLeft = expr
    COMMA bindRight = VARIABLE ARROW eRight = expr RPAREN
    { TCase (scrutineeTm, bindLeft, eLeft, bindRight, eRight) }
  (* Let bindings *)
  | LET bnd = VARIABLE EQUALS bndTm = expr IN coreTm = expr 
    { TLet (bnd, bndTm, coreTm) }
  (* Fixed points *)
  | FIX e = expr COLON t = ty { TFix (e, t) }
  (* Conditional flow *)
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { TIf (e1, e2, e3) }
  (* Pairs *)
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { TProduct (e1, e2) }
  (* Sequencing *)
  | e1 = expr SEMICOLON e2 = expr { TSequence (e1, e2) }
  (* Session stuff *)
  | SEND e1 = expr e2 = expr { TSend (e1, e2) }
  | RECEIVE e = expr { TReceive e }
  | FORK e = expr { TFork e }
  | WAIT e = expr { TWait e }

operator: 
  | LANGLE { Lt } | LE { Le } 
  | RANGLE { Gt } | GE { Ge } 
  | EQ { Eq }     | NEQ { Neq } 
  | PLUS { Plus } | MINUS { Minus } 
  | STAR { Mult } | FSLASH { Div }

binary_operation:
  | e1 = binary_operation 
    o = operator 
    e2 = binary_operation { TBinOp (o, e1, e2) }
  | f = fact { f }

fact:
  | b = BOOL { TConstant (CBoolean b) }
  | v = VARIABLE { TVariable v }
  | i = INT { TConstant (CInteger i) }
  (* Parenthesised expression *)
  | LPAREN e = expr RPAREN { e }

(* Type parser *)
ty:
  | LANGLE t = sess_ty RANGLE { Session t }
  | UNIT { Unit }
  | t1 = ty ARROW t2 = ty { Arrow (t1, t2) } 
  | t1 = ty LOLLI t2 = ty { LinearArrow (t1, t2) }
  | t1 = ty PLUS t2 = ty { Sum (t1, t2) }
  | t1 = ty STAR t2 = ty { Product (t1, t2) }
  | t = base_ty { t }

nary_sess_ty:
  | s1 = sess_ty COMMA s2 = nary_sess_ty { s1 :: s2 }
  | s = sess_ty { s :: [] }
				  
sess_ty:
  | ENDBANG { SendEnd }
  | ENDQUERY { ReceiveEnd }
  | BANG t1 = ty DOT t2 = sess_ty { Send (t1, t2) }
  | QSTNMARK t1 = ty DOT t2 = sess_ty { Receive (t1, t2) }
  | LPAREN PLUS RPAREN t = nary_sess_ty { SendChoice t }
  | LPAREN AMPERSAND RPAREN t = nary_sess_ty { ReceiveChoice t }

base_ty:
  | TYINT  { Base Integer }
  | TYBOOL { Base Boolean }

(* Entrypoint *)
expr_main:
  | e = expr EOF { e }

