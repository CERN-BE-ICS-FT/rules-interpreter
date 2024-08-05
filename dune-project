(lang dune 3.16)

(name rules_component)

(generate_opam_files true)

(package
 (name rules_component)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (grpc-lwt (>= 0.1.0))
  (h2-lwt-unix (>= 0.9.0))
  (ocaml-protoc-plugin (>= 4.4.0))
  ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
