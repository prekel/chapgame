(lang dune 2.7)

(name chapgame)

(version 0.1)

(license MIT)

(authors "Vladislav Prekel")

(maintainers "Vladislav Prekel")

(generate_opam_files true)

(package
 (name bonsai)
 (synopsis "Bonsai")
 (description "(vendored)")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  core
  virtual_dom
  incr_dom
  incr_map
  async_kernel))

(package
 (name chapgame)
 (synopsis "A short, but powerful statement about your project")
 (description "A short, but powerful statement about your project")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  core
  lwt
  js_of_ocaml
  uuidm
  dream
  cohttp-lwt-jsoo
  dream-encoding
  brr
  (crunch :build)
  (ppx_jane :build)
  (bisect_ppx :with-test)
  (ocaml-lsp-server :with-test)
  (ocamlformat :with-test)
  (ocamlformat-rpc :with-test)
  (utop :with-test)
  (odoc :with-doc)))
