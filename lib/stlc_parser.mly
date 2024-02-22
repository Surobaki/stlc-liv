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

%token PLUS MINUS STAR FSLASH
%token GE GT LE LT EQ NEQ

%left LT GT GE LE EQ NEQ
%left PLUS MINUS
%left STAR FSLASH

(* Start parsing *)
%start <livTerm> expr_main

%%

expr:
    (* Let *)
(*
  Not using let atm
    | LET VARIABLE type_annot? EQ expr IN expr
        { Let { binder = $2; annot = $3; term = $5; body = $7 } }
    | LET LEFT_PAREN VARIABLE COMMA VARIABLE RIGHT_PAREN EQ basic_expr IN expr
        { LetPair { binders = ($3, $5); term = $8; cont = $10 } }
    | basic_expr SEMICOLON expr { Seq ($1, $3) }
    | basic_expr COLON ty { Annotate ($1, $3) }
*)
  | e = basic_expr { e }

basic_expr:
  (* Lam *)
  | LAMBDA v = VARIABLE COLON t = ty DOT e = basic_expr
    { TAbstract (v, t, e) }
  (* App *)
  | e1 = fact e2 = basic_expr { TApplication (e1, e2) }
  (* Binary operators *)
  | o = operator { o }

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

