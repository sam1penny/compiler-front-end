(lang dune 3.4)

(name compiler)

(generate_opam_files true)

(source
 (github sam1penny/compiler))

(authors "Sam Penny")

(license LICENSE)

(package
 (name compiler)
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
