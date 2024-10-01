# Use ocaml/opam:ubuntu-20.04-ocaml-5.4 as the base image
# FROM ocaml/opam:ubuntu-20.04-ocaml-5.4
FROM ocaml/opam:debian-11-ocaml-5.4

RUN opam install dune yojson

RUN opam init

RUN eval $(opam env)

CMD ["bash"]