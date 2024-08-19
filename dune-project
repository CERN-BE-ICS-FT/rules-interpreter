(lang dune 3.16)

(name rules_component)

(generate_opam_files true)

(package
 (name rules_component)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (yojson (= 2.2.2))
  ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
