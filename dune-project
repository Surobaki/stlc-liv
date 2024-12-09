(lang dune 3.13)
(using menhir 2.1)

(name Ntextual)

(generate_opam_files true)

(source
 (github Surobaki/stlc-liv))

(authors "Olivia Weston")

(maintainers "Olivia Weston")

(license LICENSE)

(package
 (name Ntextual)
 (synopsis "A co-contextual lambda calculus with sessions")
 (description "A co-contextual type checker (alongside a basic parser and lexer) for a session-typed language.")
 (depends
   (ocaml (>= 5.3))
   dune
   menhir
   )
 (tags
  (topics pl pldi stlc "lambda calculus" typechecker interpreter)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
