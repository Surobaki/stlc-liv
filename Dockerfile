FROM ocaml/opam:ubuntu-25.04-ocaml-4.14
WORKDIR /usr/local/ntextual

COPY --chown=opam bin /usr/local/ntextual/bin
COPY --chown=opam lib /usr/local/ntextual/lib
COPY --chown=opam test /usr/local/ntextual/test
COPY --chown=opam LICENSE /usr/local/ntextual
COPY --chown=opam README_Programming.md /usr/local/ntextual
COPY --chown=opam Makefile /usr/local/ntextual
COPY --chown=opam dune-project /usr/local/ntextual

RUN <<EOF
opam update --yes
opam install --yes dune menhir cmdliner
EOF

CMD ["/bin/bash"]
