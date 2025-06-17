# Co-contextualised Session Types
Hi! This is the repository which contains my experiments with writing the co-contextual versions of GV style session typed languages, and their many facets.

# Dependencies
Check `dune-project`. At the moment this project uses:
- `menhir` (parsing)
- `ocamllex` (lexing)

# Roadmap / Ideamap
1. ~~Write core constructions of STLC using OCaml.~~
2. ~~Write an environment-based interpreter.~~
3. ~~Generate a parser with Menhir.~~
4. ~~Write a tree-walking type checker with annotated lambdas.~~
5. ~~Add let-bindings, if-conditions and fixed points to transform STLC into PCF.~~
6. ~~Write a co-contextual typechecker.~~
7. ~~Write unification algorithm to process co-contextual typechecker's output triple.~~
8. ~~Write documentation for co-contextual typechecker with formal derivation rules.~~
9. ~~Write linear versions of co-contextual rules.~~

# Products
A co-contextual type checker binary file ran as a CLI utility.

# Quick overview
The language is in `lib/ast.ml`, the type checker is in `lib/cctx_typechecker.ml`, the tests are in `test` (run `python run-tests.py` from within the `test` directory). The parser is `lib/parser.mly`, the lexer is `lib/lexer.mll`. The commandline entrypoint is in `bin/main.ml`.

# Credit
A lot of this is for my PhD at University of Glasgow, so a lot of it is copypasted from my supervisor's project. For a more refined look at various typing practices I recommend looking at [Simon Fowler's type-system-implementations](https://github.com/SimonJF/type-system-implementations) and [Simon's typechecker for mailbox typing in Pat](https://github.com/SimonJF/mbcheck).
