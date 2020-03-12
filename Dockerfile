FROM ocaml/opam2:alpine
ENV OPAMJOBS 8
WORKDIR /src
COPY . /src
RUN opam pin add mirage-block-unix /src -n
RUN opam depext -u -i mirage-block-unix
RUN opam install mirage-block-unix -y
