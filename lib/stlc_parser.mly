%{
  open Stlc
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token LAMBDA COLON ARROW
%token LPAREN RPAREN
%token EOF

%start <livTerm> main

%right ARROW

%%

main:
  | e = expr EOF { e }

expr:
  | LAMBDA bind = VAR e = expr { TAbstract (bind, e) }
  | f = fact e = expr { TApplication (f, e) }

fact:
  | b = BOOL { TConstant (CBoolean b) }
  | i = INT { TConstant (CInteger i) }
  | LPAREN e = expr RPAREN { e }

  
