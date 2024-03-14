%{
  open Stlc
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
%token EOF
%token LET
%token EQUALS
%token IN
%token FIX
%token IF
%token THEN
%token ELSE

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
  (* App *)
  | e1 = fact e2 = expr { TApplication (e1, e2) }
  (* Binary operators *)
  | o = operator { o }
  (* Let bindings *)
  | LET bnd = VARIABLE EQUALS bndTm = expr IN coreTm = expr 
    { TLet (bnd, bndTm, coreTm) }
  (* Fixed points *)
  | FIX e = expr COLON t = ty { TFix (e, t) }
  (* Conditional flow *)
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { TIf (e1, e2, e3) }

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
  | t1 = base_ty ARROW t2 = ty { Arrow (t1, t2) } 
  | t = base_ty { t }

base_ty:
  | TYINT  { Integer }
  | TYBOOL { Boolean }

expr_main:
  | e = expr EOF { e }

