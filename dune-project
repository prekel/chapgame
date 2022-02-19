(lang dune 2.7)

(name chapgame)
(version 2d43c1d)

(license MIT)

(authors "Vladislav Prekel")

(maintainers "Vladislav Prekel")

(generate_opam_files true)

(package
 (name chapgame)
 (synopsis "A short, but powerful statement about your project")
 (description "A short, but powerful statement about your project")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  (core :build)
  (lwt :build)
  js_of_ocaml
  (bonsai (> v0.14))
  (ppx_jane :build)
  (bisect_ppx :with-test)
  (ocaml-lsp-server :with-test)
  (ocamlformat :with-test)
  (ocamlformat-rpc :with-test)
  (utop :with-test)
  (odoc :with-doc)))
