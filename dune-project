(lang dune 3.16)

(name OCamlEvaluator)

(generate_opam_files true)

(source
 (github eric5512/OCamlEvaluator))

(authors "Eric D'Andria")

(maintainers "Eric D'Andria")

(license LICENSE)

(using menhir 2.0)

(package
 (name OCamlEvaluator)
 (synopsis "Mathematical expression evaluator")
 (description "A mathematical expresion evaluator writen in ocaml. It's capable of defining variables and functions, derivate, solve equations and plot functions")
 (depends ocaml dune mehir oplot))
