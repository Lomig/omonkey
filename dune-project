(lang dune 3.16)

(name omonkey)

(generate_opam_files true)

(source
 (github Lomig/omonkey))

(authors "Lomig")

(maintainers "Lomig")


(package
 (name omonkey)
 (synopsis "A Monkey Interpreter")
 (description "A Monkey Interpreter as described in 'Writing an Interpreter in Go' by Thorsten Ball")
 (depends ocaml dune)
 (tags
  (topics ocaml interpreter monkey fun)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
