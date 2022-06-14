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
  | Done s -> s
  | Raw s -> sprintf "%s \\TODO" s
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

let content =
  [ Done
      {|\begingroup
\renewcommand{\section}[2]{\Anonchapter{Список использованных источников}\vspace{-1em}}
\begin{thebibliography}{00}
|}
  ; Raw
      {|\bibitem{kant}
    Критика чистого разума
    Сочинения в шести томах.
    Том 3
    Академия наук СССР
    Институт философии
    Издательство социально-экономической литературы
    «Мысль»
    Москва - 1964. \TODO
    |}
  ; Done
      {|\bibitem{rowellherbert}
    Роуэлл, Г. Физика : учебное издание / Г. Роуэлл, С. Герберт. -- Москва : Просвещение, 1994. -- 576 с. -- ISBN 5-09-002920-2.
    |}
  ; Raw
      {|\bibitem{mathforprogrammers}
    Math for Progammers \TODO
    https://www.manning.com/books/math-for-programmers
    |}
  ; Raw
      {|\bibitem{larson}
    Larson R., Hostetler R. . Precalculus: A Concise Course. — Boston:
    Houghton Mifflin, 2007. — xvii + 526 + 102 p. — ISBN 0-618-62719-7. \TODO
    |}
  ; Raw
      {|\bibitem{alekseevabel}
    Алексеев, В. Б. Теорема Абеля в задачах и решениях. — М.: МЦНМО, 2001. — 192 с. — ISBN 5-900916-86-3. \TODO
    |}
  ; Raw
      {|\bibitem{wiki-ellastic-collision}
    https://en.wikipedia.org/wiki/Elastic\_collision \TODO
    |}
  ; Raw
      {| \bibitem{bisectionkaw}
    \TODO Autar K Kaw Numerical Methods with Applications Chapter 03.03 Bisection Method
    |}
  ; Raw
      {|\bibitem{mdn-spa}
    https://developer.mozilla.org/en-US/docs/Glossary/SPA \TODO
    |}
  ; Raw
      {|\bibitem{wasm}
    Andreas Haas, Andreas Rossberg, Derek L. Schuff, Ben L. Titzer, Michael Holman, Dan Gohman, Luke Wagner, Alon Zakai, and JF Bastien.
    2017. Bringing the web up to speed with WebAssembly.
    In Proceedings of the 38th ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI 2017).
    Association for Computing Machinery, New York, NY, USA, 185–200. https://doi.org/10.1145/3062341.3062363 \TODO
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
  ; Raw
      {|\bibitem{dsyme-hopl}
    https://fsharp.org/history/hopl-final/hopl-fsharp.pdf
    Don Syme. 2020. The early history of F\#. Proc. ACM Program. Lang. 4, HOPL, Article 75 (June 2020), 58 pages. https://doi.org/10.1145/3386325
    \TODO
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
  ; Raw
      {|\bibitem{yaron2011}
    Yaron Minsky. OCaml for the Masses. ACM Queue, Sep 27, 2011 \TODO
    |}
  ; Raw
      {|\bibitem{rwo-prologue}
    Real World OCaml. 2nd Edition. Yaron Minsky. Anil Madhavapeddy.
    https://dev.realworldocaml.org/prologue.html \TODO
    |}
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
  ; Raw
      {|\bibitem{rwo-backend}
    https://dev.realworldocaml.org/compiler-backend.html \TODO
    |}
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
  ; Raw
      {|\bibitem{rwo-ru}
    Мински Я., Мадхавапедди А., Хикки Дж.
    М57 Программирование на языке OCaml / пер. с анг.л А. Н. Киселева. –
    М.: ДМК Пресс, 2014. – 536 с.: ил.
    ISBN 978-5-97060-102-0
    \TODO
    |}
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
  ; Raw {|\bibitem{rwo-testing}
    https://dev.realworldocaml.org/testing.html
    |}
  ; Raw
      {|\bibitem{rwo-runtime-memory}
    https://dev.realworldocaml.org/runtime-memory-layout.html \TODO

    |}
  ; Raw {|\bibitem{rwo-json}
    https://dev.realworldocaml.org/json.html \TODO
    |}
  ; Raw
      {|\bibitem{rwo-sexp}
    https://dev.realworldocaml.org/data-serialization.html \TODO
    |}
  ; Raw
      {|\bibitem{anil-gemma-qcon}
    https://www.infoq.com/presentations/ocaml-browser-iot/ \TODO
    |}
  ; Raw
      {|\bibitem{rwo-platform}
    https://dev.realworldocaml.org/platform.html \TODO
    |}
  ; Raw
      {|\bibitem{bulma-vs-bootstrap}
    https://bulma.io/alternative-to-bootstrap/ \TODO
    |}
  ; Raw
      {|\bibitem{wiki-mlmodules}
    https://ru.wikipedia.org/wiki/Язык\_модулей\_ML
    |}
  ; Raw
      {|\bibitem{functor-driven}
    https://arxiv.org/abs/1905.02529 Programming Unikernels in the Large via Functor Driven
    Development (Experience Report) \TODO Radanne G. et al. Programming unikernels in the large via functor driven development //arXiv preprint arXiv:1905.02529. – 2019.
    |}
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
  ; LabeledRaw
      { label = "rwo-gadt"; raw = {|https://dev.realworldocaml.org/gadts.html \TODO|} }
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
