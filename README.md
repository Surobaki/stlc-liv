# Olivia's Simply Typed Lambda Calculus
Hi! This is the repository which contains my experiments with writing the Simply Typed Lambda Calculus and its many facets.

# Dependencies
Check `dune-project`. At the moment this project uses:
- `menhir` (parsing)
- `ocamllex` (lexing)
- `ppx_deriving.show` (easy pretty printing of AST)

# Roadmap / Ideamap
1. ~~Write core constructions of STLC using OCaml.~~
2. Write a substitution based interpreter.
3. ~~Generate a parser with Menhir.~~
4. Write a tree-walking type checker with annotated lambdas.

# Products
1. Core constructions of STLC can be found in `lib/stlc.ml`. The access point to the RPPL (Read Parse Print Loop) is `bin/main.ml`.
3. The parser's description is in `lib/stlc_parser.mly`, the lexer specification is in `lib/stlc_lexrules.mll` and the parsing wrapper that gives functions interacting within standard input/output is in `lib/parse_wrapper.ml`.
4. The type checker can be found in `lib/typechecker.ml`(`.mli`).
