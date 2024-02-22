(lang dune 3.13)
(using menhir 2.1)

(name STLC)

(generate_opam_files true)

(source
 (github Surobaki/stlc-liv))

(authors "Olivia Weston")

(maintainers "Olivia Weston")

(license LICENSE)

(package
 (name STLC)
 (synopsis "Implementation of Simply Typed Lambda Calculus")
 (description "A Simply Typed Lambda Calculus with an interpreter and type checker")
 (depends
   ocaml
   dune
   menhir
   )
 (tags
  (topics pl pldi stlc "lambda calculus" typechecker interpreter)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
