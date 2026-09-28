%{
  open Ast
%}
(* Tokens *)
(* Primitive variable tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> VARIABLE

(* Parentheses *)
%token LPAREN RPAREN

(* Type construction grammar *)
%token ARROW 
%token DOT
%token LANGLE RANGLE

(* Common language constructions *)
%token IF THEN ELSE
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

(* All precedence preferences *)
%nonassoc GE LE LANGLE RANGLE EQ NEQ

%left MINUS PLUS
%left FSLASH STAR

(* Assigning OCaml types to nonterminals *)
%type <term> expr binary_operation app fact

(* Start parsing *)
%start <term> expr_main

%%

(* Loosest layer *)
expr:
  (* Lam *)
  | LAMBDA v = VARIABLE DOT e = expr
    { TAbstract (v, e) }
  (* LinLam *)
  | LAMBDA v = VARIABLE DOT AT e = expr
    { TLinAbstract (v, e) }
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
  (* Conditional flow *)
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { TIf (e1, e2, e3) }
  (* Sequencing *)
  | e1 = app SEMICOLON e2 = expr { TSequence (e1, e2) }
  (* Session stuff *)
  | SEND e1 = fact e2 = expr { TSend (e1, e2) }
  | RECEIVE e = expr { TReceive e }
  | FORK e = fact { TFork e }
  | WAIT e = fact { TWait e }
  (* Binary binary_operations *)
  | o = binary_operation { o }

(* Application binds tighter than binops and prefixes *)
app:
  | e1 = app e2 = fact { TApplication (e1, e2) }
  | f = fact { f }

binary_operation:
  | e1 = binary_operation PLUS    e2 = binary_operation { TBinOp (Plus,  e1, e2) }
  | e1 = binary_operation MINUS   e2 = binary_operation { TBinOp (Minus, e1, e2) }
  | e1 = binary_operation STAR    e2 = binary_operation { TBinOp (Mult,  e1, e2) }
  | e1 = binary_operation FSLASH  e2 = binary_operation { TBinOp (Div,   e1, e2) }
  | e1 = binary_operation LANGLE  e2 = binary_operation { TBinOp (Lt,    e1, e2) }
  | e1 = binary_operation RANGLE  e2 = binary_operation { TBinOp (Gt,    e1, e2) }
  | e1 = binary_operation LE      e2 = binary_operation { TBinOp (Le,    e1, e2) }
  | e1 = binary_operation GE      e2 = binary_operation { TBinOp (Ge,    e1, e2) }
  | e1 = binary_operation EQ      e2 = binary_operation { TBinOp (Eq,    e1, e2) }
  | e1 = binary_operation NEQ     e2 = binary_operation { TBinOp (Neq,   e1, e2) }
  | a = app { a }

fact:
  | b = BOOL { TConstant (CBoolean b) }
  | v = VARIABLE { TVariable v }
  | i = INT { TConstant (CInteger i) }
  | LPAREN RPAREN { TUnit }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { TProduct (e1, e2) }
  (* Parenthesised expression *)
  | LPAREN e = expr RPAREN { e }

(* Entrypoint *)
expr_main:
  | e = expr EOF { e }
