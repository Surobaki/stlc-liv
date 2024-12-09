%{
  open Stlc
  open List
  (* let parse_error x = Errors.Parse_error x *)
%}
(* Tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> VARIABLE
%token TYINT
%token TYBOOL

%token LPAREN RPAREN
%token COLON
%token DOT
%token LAMBDA
%token ARROW
%token AT
%token LOLLI
%token COMMA
%token MATCH
%token WITH
%token SEMICOLON
%token BANG
%token QSTNMARK
%token FORK
%token WAIT
%token SEND
%token RECEIVE
%token INL
%token INR
%token ENDBANG
%token ENDQUERY
%token EOF
%token LET
%token EQUALS
%token IN
%token FIX
%token IF
%token THEN
%token ELSE
%token UNIT
%token AMPERSAND
%token LANGLE RANGLE

%token PLUS MINUS STAR FSLASH
%token GE GT LE LT EQ NEQ

%left LT GT GE LE EQ NEQ
%left PLUS MINUS
%left STAR FSLASH

(* Start parsing *)
%start <livTerm> expr_main

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
  (* Binary operators *)
  | o = operator { o }
  (* Let product elimination *)
  | LET LPAREN v1 = VARIABLE COMMA v2 = VARIABLE RPAREN EQUALS prodTm = expr
    IN continTm = expr
    { TLetProduct (v1, v2, prodTm, continTm) }
  (* Sum injections *)
  | INL e = expr { TInL e }
  | INR e = expr { TInR e }
  (* Sum elimination *)
  | MATCH scrutineeTm = expr WITH LPAREN bindLeft = VARIABLE ARROW eLeft = expr
    COMMA bindRight = VARIABLE ARROW eRight = expr RPAREN
    { TCase (scrutineeTm, bindLeft, eLeft, bindRight, eRight) }
  (* Sequencing *)
  | e1 = expr SEMICOLON e2 = expr { TSequence (e1, e2) }
  (* Let bindings *)
  | LET bnd = VARIABLE EQUALS bndTm = expr IN coreTm = expr 
    { TLet (bnd, bndTm, coreTm) }
  (* Fixed points *)
  | FIX e = expr COLON t = ty { TFix (e, t) }
  (* Conditional flow *)
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { TIf (e1, e2, e3) }
  (* Pairs *)
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { TProduct (e1, e2) }
  (* Session stuff *)
  (* Send *)
  | SEND e1 = expr e2 = expr { TSend (e1, e2) }
  (* Receive *)
  | RECEIVE e = expr { TReceive e }
  (* Fork *)
  | FORK e = expr { TFork e }
  (* Wait *)
  | WAIT e = expr { TWait e }

operator:
  | e1 = operator LT e2 = operator     { TBinOp (Lt, e1, e2) }
  | e1 = operator LE e2 = operator     { TBinOp (Le, e1, e2) }
  | e1 = operator GT e2 = operator     { TBinOp (Gt, e1, e2) }
  | e1 = operator GE e2 = operator     { TBinOp (Ge, e1, e2) }
  | e1 = operator EQ e2 = operator     { TBinOp (Eq, e1, e2) }
  | e1 = operator NEQ e2 = operator    { TBinOp (Neq, e1, e2) }
  | e1 = operator PLUS e2 = operator   { TBinOp (Plus, e1, e2) }
  | e1 = operator MINUS e2 = operator  { TBinOp (Minus, e1, e2) }
  | e1 = operator STAR e2 = operator   { TBinOp (Mult, e1, e2) }
  | e1 = operator FSLASH e2 = operator { TBinOp (Div, e1, e2) }
  | f = fact { f }

fact:
  | b = BOOL { TConstant (CBoolean b) }
  (* Var *)
  | v = VARIABLE { TVariable v }
  (* Constant *)
  | i = INT { TConstant (CInteger i) }
  (* Parenthesised expression *)
  | LPAREN e = expr RPAREN { e }

(* Type parser *)
ty:
  | t1 = ty ARROW t2 = ty { Arrow (t1, t2) } 
  | t1 = ty LOLLI t2 = ty { LinearArrow (t1, t2) }
  | t1 = ty PLUS t2 = ty { Sum (t1, t2) }
  | t1 = ty STAR t2 = ty { Product (t1, t2) }
  | LANGLE t = sess_ty RANGLE { Session t }
  | UNIT { Unit }
  | t = base_ty { t }
				  
sess_ty:
  | BANG t1 = base_ty DOT t2 = sess_ty { Send (t1, t2) }
  | QSTNMARK t1 = base_ty DOT t2 = sess_ty { Receive (t1, t2) }
  | PLUS t1 = sess_ty COMMA t2 = sess_ty 
    { SendChoice (cons t1 (cons t2 [])) }
  | AMPERSAND t1 = sess_ty COMMA t2 = sess_ty 
    { ReceiveChoice (cons t1 (cons t2 [])) }
  | ENDBANG { SendEnd }
  | ENDQUERY { ReceiveEnd }

base_ty:
  | TYINT  { Base Integer }
  | TYBOOL { Base Boolean }

expr_main:
  | e = expr EOF { e }

