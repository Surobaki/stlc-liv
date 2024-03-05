# Olivia's Simply Typed Lambda Calculus
Hi! This is the repository which contains my experiments with writing the Simply Typed Lambda Calculus and its many facets.

# Dependencies
Check `dune-project`. At the moment this project uses:
- `menhir` (parsing)
- `ocamllex` (lexing)
- `ppx_deriving.show` (easy pretty printing of AST)

# Roadmap / Ideamap
1. ~~Write core constructions of STLC using OCaml.~~
2. Write an environment-based interpreter.
3. ~~Generate a parser with Menhir.~~
4. ~~Write a tree-walking type checker with annotated lambdas.~~

# Products
1. Core constructions of STLC can be found in `lib/stlc.ml`. The access point to the RPPL (Read Parse Print Loop) is `bin/main.ml`.
3. The parser's description is in `lib/stlc_parser.mly`, the lexer specification is in `lib/stlc_lexrules.mll` and the parsing wrapper that gives functions interacting within standard input/output is in `lib/parse_wrapper.ml`.
4. The type checker can be found in `lib/typechecker.ml`(`.mli`).
5. The interpreter can be foudn in `lib/interpreter.ml`(`.mli`).

# Running this thing
Frankly there's no nice interface to run this. I recommend either running functions directly via OCaml's `utop` or alternatively, go into `bin/main.ml` and pick your favourite loop (`rppl` stands for read-parse-print-loop, `rptpl` stands for read-parse-typecheck-print-loop, `repl` coming soon). Once the loop is initiated, just input your favourite lambda terms, e.g. `(\x:Int.x)5`.

# Credit
A lot of this is for my PhD at University of Glasgow, so a lot of it is copypasted from my supervisor's project. For a more refined look at various typing practices I recommend looking at [Simon Fowler's type-system-implementations](https://github.com/SimonJF/type-system-implementations) and [Simon's typechecker for mailbox typing in Pat](https://github.com/SimonJF/mbcheck).
