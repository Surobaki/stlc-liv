# Olivia's Simply Typed Lambda Calculus
Hi! This is the repository which contains my experiments with writing the Simply Typed Lambda Calculus and its many facets. It's actually not the Simply Typed Lambda Calculus anymore because I've added a lot more features, but I'll call it Simply Typed Liv Calculus to differentiate it from the original. The language is more accurately something closer to PCF.

# Dependencies
Check `dune-project`. At the moment this project uses:
- `menhir` (parsing)
- `ocamllex` (lexing)
- `ppx_deriving.show` (easy pretty printing of AST)

# Roadmap / Ideamap
1. ~~Write core constructions of STLC using OCaml.~~
2. ~~Write an environment-based interpreter.~~
3. ~~Generate a parser with Menhir.~~
4. ~~Write a tree-walking type checker with annotated lambdas.~~
5. ~~Add let-bindings, if-conditions and fixed points to transform STLC into PCF.~~
6. ~~Write a co-contextual typechecker.~~
7. ~~Write unification algorithm to process co-contextual typechecker's output triple.~~
8. Write documentation for co-contextual typechecker with formal derivation rules.
9. Write linear versions of co-contextual rules.

# Products
1. Core constructions of STLC can be found in `lib/stlc.ml`. The access point to the various REPL-esque loops is `bin/main.ml`.
3. The parser's description is in `lib/stlc_parser.mly`, the lexer specification is in `lib/stlc_lexrules.mll` and the parsing wrapper that gives functions interacting within standard input/output is in `lib/parse_wrapper.ml`.
4. The type checker can be found in `lib/typechecker.ml`(`.mli`).
5. The interpreter can be found in `lib/interpreter.ml`(`.mli`). It has auxiliary definitions in `lib/interpreter_aux.ml`(`.mli`).
6. The co-contextual typechecker can be found in `lib/cctx_typechecker.ml`(`.mli`).
7. The `doc` folder will contain additional documentation in the form of a LaTeX project.

# Running this thing
Frankly there's no nice interface to run this. I recommend either running functions directly via OCaml's `utop` or alternatively, run the main executable (`dune exec STLC`) and pick your favourite loop (`rppl` stands for read-parse-print-loop, `rptpl` stands for read-parse-typecheck-print-loop, `repl` stands for read-eval-print-loop). Once the loop is initiated, just input your favourite lambda terms, e.g. `(\x:Int.x)5`. Don't forget that if you're entering a string into `utop` then you should escape your lambda backslashes, i.e. `\x:Int` -> `\\x:Int`. Support for custom type and value environments is not built into the executable so you'll have to use `utop` for that.

Currently the full REPL runs both a normal and a co-contextual typechecker, providing outputs of both to compare.

# Credit
A lot of this is for my PhD at University of Glasgow, so a lot of it is copypasted from my supervisor's project. For a more refined look at various typing practices I recommend looking at [Simon Fowler's type-system-implementations](https://github.com/SimonJF/type-system-implementations) and [Simon's typechecker for mailbox typing in Pat](https://github.com/SimonJF/mbcheck).
