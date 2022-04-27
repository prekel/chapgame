FROM ocaml/opam:alpine-ocaml-4.13 as build

# Install system dependencies
WORKDIR /home/opam

# Install dependencies
ADD chapgame.opam chapgame.opam
RUN opam-dev install --deps-only . -y

# Build project
ADD . .
RUN opam-dev exec -- dune build --release ./server/main.exe


FROM alpine as run

RUN apk add --update libev

COPY --from=build /home/opam/_build/default/server/main.exe /bin/app

EXPOSE 8080
ENTRYPOINT /bin/app
