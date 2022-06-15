open Core

type t =
  | Done of string
  | Raw of string
  | LabeledRaw of
      { label : string
      ; raw : string
      }
  | EDN of
      { label : string
      ; raw_before_edn : string
      ; edn : string
      }

let print = function
  | Done s -> [%string "%{s}"]
  | Raw s -> sprintf "%s \\TODO\n" s
  | LabeledRaw { label; raw } -> sprintf {|\bibitem{%s}
    \TODO %s
    |} label raw
  | EDN { label; raw_before_edn; edn } ->
    sprintf
      {|\bibitem{%s}
      %s -- EDN \href{https://www.elibrary.ru/%s}{%s}.
      |}
      label
      raw_before_edn
      edn
      edn
;;

let print_list list = list |> List.map ~f:print |> String.concat ~sep:"\n"

let rwo2nd ~label ~title ~url =
  Done
    [%string
      {|\bibitem{%{label}}
      Minsky,~Y. %{title} : Real World OCaml~/
      Yaron Minsky, Anil Madhavapeddy~//
      Real World OCaml : Functional programming for the masses (2nd Edition).~--
      URL: \underline{\smash{\href{%{url}}{%{url}}}} (дата обращения: 25.05.2022).
      |}]
;;

let arxiv ~label ~arxiv ~title ~author ~authors ~year ~pubdate =
  Done
    [%string
      {|\bibitem{%{label}}
    %{author} %{title}~/ %{authors}.~--
    %{year}.~--
    arXiv: \href{https://arxiv.org/abs/%{arxiv}}{%{arxiv}}.~--
    URL: \underline{\smash{\href{https://arxiv.org/pdf/%{arxiv}.pdf}{https://arxiv.org/pdf/%{arxiv}.pdf}}}.~--Publication date: %{pubdate}.
    |}]
;;

let content =
  [ Done
      {|\begingroup
\renewcommand{\section}[2]{\Anonchapter{Список использованных источников}\vspace{-1em}}
\begin{thebibliography}{00}
|}
  ; Done
      {|\bibitem{kant}
      Кант,~И. Сочинения в шести томах. Том 3.~/ И. Кант. [Под общ.~ред. В.~Ф.~Асмуса, А.~В.~Гулыги, Т.~И.~Ойзермана.]~--
      Москва~: Мысль, 1964.~--
      799~с.
    |}
  ; Done
      {|\bibitem{rowellherbert}
      Роуэлл,~Г. Физика~: учебное издание~/ Г.~Роуэлл, С.~Герберт.~--
      Москва~: Просвещение, 1994.~--
      576~с.~--
      ISBN 5-09-002920-2.
      |}
  ; Done
      {|\bibitem{mathforprogrammers}
      Orland,~P. Math for Progammers~: 3D graphics, machine learning, and simulations with Python~/ P. Orland.~--
      Shelter Island, NY~: Manning, 2020.~--
      688~p.~--
      ISBN 978-1617295355.
      |}
  ; Done
      {|\bibitem{larson}
      Larson,~R. Precalculus~: a concise course~/ R.~Larson, R.~Hostetler~--
      Boston~: Houghton Mifflin, 2007.~--
      656~p.~--
      ISBN 0-618-62719-7.
      |}
  ; Done
      {|\bibitem{alekseevabel}
      Алексеев,~В.~Б. Теорема Абеля в задачах и решениях~/ В.~Б.~Алексеев.~--
      Москва~: МЦНМО, 2001.~--
      192~с.~--
      ISBN 5-900916-86-3.
      |}
  ; Raw
      {|\bibitem{wiki-ellastic-collision}
      https://en.wikipedia.org/wiki/Elastic\_collision \TODO
      |}
  ; Done
      {|\bibitem{bisectionkaw}
      Autar Kaw. Bisection Method of Solving a Nonlinear Equation~: [textbook chapter]~/ Autar K Kaw //
      Textbook: Numerical Methods with Applications.~--
      URL: \underline{\smash{\href{https://nm.mathforcollege.com/mws/gen/03nle/mws\_gen\_nle\_txt\_bisection.pdf}{https://nm.mathforcollege.com/mws/gen/03nle/mws\_gen\_nle\_txt\_bisection.pdf}}}.~--
      Publication date: 15.01.2012.
      |}
  ; Raw
      {|\bibitem{mdn-spa}
    https://developer.mozilla.org/en-US/docs/Glossary/SPA \TODO
    |}
  ; Done
      {|\bibitem{wasm}
      Haas,~A. Bringing the web up to speed with WebAssembly~/
      Andreas Haas, Andreas Rossberg, Derek L. Schuff [et~al.].~--
      DOI~\href{https://doi.org/10.1145/3062341.3062363}{10.1145/3062341.3062363}~//
      In Proceedings of the 38th ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI 2017).~--
      2017.~--
      P.~185--200.~--
      URL: \underline{\smash{\href{https://dl.acm.org/doi/pdf/10.1145/3062341.3062363}{https://dl.acm.org/doi/pdf/10.1145/3062341.3062363}}}.~--
      Publication date: 14.06.2017.
      |}
  ; Raw
      {|\bibitem{emscripten-about}
    https://emscripten.org/docs/introducing\_emscripten/about\_emscripten.html \TODO
    |}
  ; Raw
      {|\bibitem{rust-wasm}
    https://rustwasm.github.io/docs/book/why-rust-and-webassembly.html \TODO
    |}
  ; Raw
      {|\bibitem{blazor-ru}
    https://docs.microsoft.com/ru-ru/aspnet/core/blazor \TODO
    |}
  ; Raw {|\bibitem{fsbolero}
    https://fsbolero.io/docs/ \TODO
    |}
  ; Raw
      {|\bibitem{wasm-iwantto}
    https://webassembly.org/getting-started/developers-guide/ \TODO
    |}
  ; Done
      {|\bibitem{dsyme-hopl}
      Syme,~D. The early history of F\#~/ Don Syme.~--
      DOI~\href{https://doi.org/10.1145/3386325}{10.1145/3386325}~//
      Proceedings of the ACM on Programming Languages.~--
      2020.~--
      Vol.~4, Iss.~HOPL.~--
      P.~1--58.~--
      URL: \underline{\smash{\href{https://dl.acm.org/doi/pdf/10.1145/3386325}{https://dl.acm.org/doi/pdf/10.1145/3386325}}}.~--
      Publication date: 12.06.2020.
      |}
  ; EDN
      { label = "typescript-mayorov"
      ; raw_before_edn =
          {|Майоров, А. TypeScript для PHP-разработчика. Как писать на JavaScript большие приложения и не сойти с ума /
    А. Майоров // Системный администратор. – 2015. – № 7-8(152-153). – С. 95-99.|}
      ; edn = "UBPAGH"
      }
  ; EDN
      { label = "shutov-haskell"
      ; raw_before_edn =
          {|Шутов, В. С. Функциональное программирование для решения математических задач
    / В. С. Шутов // Фундаментальные исследования основных направлений технических и физико - математических наук
    : сборник статей Международной научно-практической конференции, Челябинск, 01 июня 2018 года.
    – Челябинск: Общество с ограниченной ответственностью "Аэтерна", 2018. – С. 120-122.|}
      ; edn = "XPWRYL"
      }
  ; Raw
      {|\bibitem{camlhistory}
    https://caml.inria.fr/about/history.en.html \TODO
    |}
  ; Raw {|\bibitem{ocamlorg}
    https://ocaml.org/ \TODO
    |}
  ; Done
      {|\bibitem{yaron2011}
      Minsky,~Y. OCaml for the Masses: Why the next language you learn should be functional~/ Yaron Minsky.~--
      DOI~\href{https://doi.org/10.1145/2030256.2038036}{10.1145/2030256.2038036}~//
      ACM Queue.~--
      2011.~--
      Vol.~9, Iss.~9.~--
      P.~40--49.~--
      URL: \underline{\smash{\href{https://dl.acm.org/doi/pdf/10.1145/2030256.2038036}{https://dl.acm.org/doi/pdf/10.1145/2030256.2038036}}}.~--
      Publication date: 27.09.2011.
      |}
  ; rwo2nd
      ~label:"rwo-prologue"
      ~title:"Prologue"
      ~url:"https://dev.realworldocaml.org/prologue.html"
  ; Raw {|\bibitem{opam}
    https://opam.ocaml.org/ \TODO
    |}
  ; Raw
      {|\bibitem{npmjs}
    https://www.npmjs.com/ \TODO Режим доступа: для зарегистрированных пользователей.
    |}
  ; Raw
      {|\bibitem{crockford}
    https://www.crockford.com/javascript/javascript.html \TODO
    |}
  ; Raw {|\bibitem{ocamljs-lambda}
    https://jaked.org/ocamljs/Jscomp.html \TODO
    |}
  ; rwo2nd
      ~label:"rwo-backend"
      ~title:"The Compiler Backend: Bytecode and Native code"
      ~url:"https://dev.realworldocaml.org/compiler-backend.html"
  ; Raw
      {|\bibitem{vouillon-jsoo}
    https://www.irif.fr/~balat/publications/vouillon\_balat-js\_of\_ocaml.pdf \TODO
    |}
  ; Raw
      {|\bibitem{bobzhang-rawlambda}
    https://github.com/ocsigen/js\_of\_ocaml/issues/338 \TODO
    |}
  ; Raw
      {|\bibitem{rescript-introduction}
    https://rescript-lang.org/docs/manual/latest/introduction \TODO
    |}
  ; Raw
      {|\bibitem{melange}
    https://anmonteiro.com/2021/03/on-ocaml-and-the-js-platform/ \TODO
    |}
  ; Raw
      {|\bibitem{ocaml-wasm}
    https://okcdz.medium.com/run-ocaml-in-the-browser-by-webassembly-31ce464594c6 \TODO
    |}
  ; Raw
      {|\bibitem{ocamlverse-libraries}
    https://ocamlverse.github.io/content/standard\_libraries.html \TODO
    |}
  ; Raw
      {|\bibitem{janestreet-opensource}
    https://opensource.janestreet.com/ \TODO
    |}
  ; rwo2nd
      ~label:"rwo-async"
      ~title:"Concurrent Programming with Async"
      ~url:"https://dev.realworldocaml.org/concurrent-programming.html"
  ; Raw
      {|\bibitem{vouillon-lwt}
    Jerome Vouillon Lwt: a Cooperative Thread Library \TODO
    https://www.irif.fr/\~vouillon/publi/lwt.pdf
    |}
  ; Raw
      {|\bibitem{announcing-async}
    https://blog.janestreet.com/announcing-async/ \TODO
    |}
  ; Raw
      {|\bibitem{rgrinberg-async}
    http://rgrinberg.com/posts/abandoning-async/ \TODO
    |}
  ; Raw
      {|\bibitem{jsoo-react}
    https://github.com/ml-in-barcelona/jsoo-react \TODO
    |}
  ; Raw
      {|\bibitem{elm-architecture}
    https://guide.elm-lang.org/architecture/ \TODO
    |}
  ; Raw {|\bibitem{lexifi-vdom}
    https://github.com/LexiFi/ocaml-vdom \TODO
    |}
  ; Raw
      {|\bibitem{janestreet-virtualdom}
    https://github.com/janestreet/virtual\_dom \TODO
    |}
  ; Raw
      {|\bibitem{bonsai-history}
    https://github.com/janestreet/bonsai/blob/master/docs/blogs/history.md \TODO
    |}
  ; Raw
      {|\bibitem{janestreet-bonsai}
    https://opensource.janestreet.com/bonsai/ \TODO
    |}
  ; Raw
      {|\bibitem{react-book}
    А. Бэнкс, Е. Порселло. React и Redux: функциональная веб-разработка ISBN 978-5-4461-0668-4 \TODO
    |}
  ; Raw
      {|\bibitem{minsky-incrdom}
    https://www.youtube.com/watch?v=R3xX37RGJKE \TODO
    |}
  ; Raw
      {|\bibitem{mdn-websocket}
    https://developer.mozilla.org/ru/docs/Web/API/WebSocket \TODO
    |}
  ; Raw {|\bibitem{dream}
    https://aantron.github.io/dream \TODO
    |}
  ; rwo2nd
      ~label:"rwo-testing"
      ~title:"Testing"
      ~url:"https://dev.realworldocaml.org/testing.html"
  ; rwo2nd
      ~label:"rwo-runtime-memory"
      ~title:"Memory Representation of Values"
      ~url:"https://dev.realworldocaml.org/runtime-memory-layout.html"
  ; rwo2nd
      ~label:"rwo-json"
      ~title:"Handling JSON Data"
      ~url:"https://dev.realworldocaml.org/json.html"
  ; rwo2nd
      ~label:"rwo-sexp"
      ~title:"Data Serialization with S-Expressions"
      ~url:"https://dev.realworldocaml.org/data-serialization.html"
  ; Raw
      {|\bibitem{anil-gemma-qcon}
    https://www.infoq.com/presentations/ocaml-browser-iot/ \TODO
    |}
  ; rwo2nd
      ~label:"rwo-platform"
      ~title:"The OCaml Platform"
      ~url:"https://dev.realworldocaml.org/platform.html"
  ; Raw
      {|\bibitem{bulma-vs-bootstrap}
    https://bulma.io/alternative-to-bootstrap/ \TODO
    |}
  ; Raw
      {|\bibitem{wiki-mlmodules}
    https://ru.wikipedia.org/wiki/Язык\_модулей\_ML
    |}
  ; arxiv
      ~label:"functor-driven"
      ~arxiv:"1905.02529"
      ~title:"Programming Unikernels in the Large via Functor Driven Development"
      ~author:"Radanne,~G."
      ~authors:"Gabriel Radanne, Thomas Gazagnaire, Anil Madhavapeddy [et~al.]"
      ~year:"2019"
      ~pubdate:"07.05.2019"
  ; LabeledRaw
      { label = "intftrick"; raw = {|https://www.craigfe.io/posts/the-intf-trick \TODO|} }
  ; Raw
      {|\bibitem{fprog-adt}
    https://www.fprog.ru/2009/issue2/practice-fp-2-compact.pdf 
    Алгебраические типы данных и их использование в программировании с. 49
    Роман Душкин \TODO
|}
  ; LabeledRaw
      { label = "mdn-wheel"
      ; raw = {|https://developer.mozilla.org/en-US/docs/Web/API/Element/wheel\_event|}
      }
  ; LabeledRaw
      { label = "jsoo-issue-1272"
      ; raw = {|https://github.com/ocsigen/js\_of\_ocaml/issues/1272|}
      }
  ; LabeledRaw
      { label = "mdnsvgtag"
      ; raw = {|https://developer.mozilla.org/en-US/docs/Web/SVG/Element/svg|}
      }
  ; rwo2nd
      ~label:"rwo-gadt"
      ~title:"GADTs"
      ~url:"https://dev.realworldocaml.org/gadts.html"
  ; LabeledRaw
      { label = "poolpi"
      ; raw =
          {|https://www.maths.tcd.ie/~lebed/Galperin.%20Playing%20pool%20with%20pi.pdf \TODO|}
      }
  ; LabeledRaw { label = "habrpi"; raw = {|https://habr.com/ru/post/533454/ \TODO|} }
  ; LabeledRaw
      { label = "browniankrugosvet"
      ; raw =
          {|https://www.krugosvet.ru/enc/nauka\_i\_tehnika/fizika/BROUNOVSKOE\_DVIZHENIE.html \TODO|}
      }
  ; LabeledRaw
      { label = "wiki-chapaev"
      ; raw =
          {|https://ru.wikipedia.org/wiki/%D0%A7%D0%B0%D0%BF%D0%B0%D0%B5%D0%B2\_(%D0%B8%D0%B3%D1%80%D0%B0) \TODO|}
      }
  ; LabeledRaw
      { label = "smeshariki-fandom"
      ; raw =
          {|https://shararam.fandom.com/wiki/%D0%A1%D0%BC%D0%B5%D1%88%D0%B0%D1%80%D0%B8%D0%BA%D0%B8\_(%D0%B8%D0%B3%D1%80%D0%B0) \TODO|}
      }
  ; LabeledRaw { label = "vk-games"; raw = {|https://dev.vk.com/games/overview \TODO|} }
  ; LabeledRaw
      { label = "tg-games"; raw = {|https://core.telegram.org/bots/games \TODO|} }
  ; LabeledRaw
      { label = "infoqmulticore"
      ; raw = {|https://www.infoq.com/news/2021/10/ocaml-5-multicore/ \TODO|}
      }
  ; LabeledRaw
      { label = "domainslibgithub"
      ; raw = {|https://github.com/ocaml-multicore/domainslib \TODO |}
      }
  ; Done {|\end{thebibliography}
\endgroup
|}
  ]
;;

let () =
  Stdio.Out_channel.with_file
    ~fail_if_exists:false
    "c2-bibliography.tex"
    ~f:Stdio.Out_channel.(fun c -> content |> print_list |> output_string c)
;;
