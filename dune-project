(lang dune 3.13)

(name STLC)

(generate_opam_files true)

(source
 (github Surobaki/stlc-liv))

(authors "Olivia Weston")

(maintainers "Olivia Weston")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name STLC)
 (synopsis "Implementation of Simply Typed Lambda Calculus")
 (description "A Simply Typed Lambda Calculus with an interpreter and type checker")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
