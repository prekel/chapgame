open Core

type t =
  | Done of string
  | Raw of string
  | LabeledRaw of
      { label : string
      ; raw : string
      }
  | Labeled of
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
  | Labeled { label; raw } -> sprintf {|\bibitem{%s}
    %s
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
      Minsky,~Y. %{title}~: Real World OCaml~/
      Yaron Minsky, Anil Madhavapeddy~//
      Real World OCaml~: Functional programming for the masses (2nd~Edition).~--
      URL: \underline{\smash{\href{%{url}}{%{url}}}} (дата~обращения: 25.05.2022).
      |}]
;;

let arxiv ~label ~arxiv ~title ~author ~authors ~year ~pubdate =
  Done
    [%string
      {|\bibitem{%{label}}
    %{author} %{title}~/ %{authors}.~--
    %{year}.~--
    arXiv: \href{https://arxiv.org/abs/%{arxiv}}{%{arxiv}}.~--
    URL: \underline{\smash{\href{https://arxiv.org/pdf/%{arxiv}.pdf}{https://arxiv.org/pdf/%{arxiv}.pdf}}}.~--
    Publication date: %{pubdate}.
    |}]
;;

let doi
    ~label
    ~doi
    ~title
    ~author
    ~authors
    ~journal
    ~yearvolumeissuepages
    ~url
    ~urlref
    ~accessdate
  =
  Done
    [%string
      {|\bibitem{%{label}}
      %{author} %{title}~/
      %{authors}.~--
  DOI~\href{https://doi.org/%{doi}}{%{doi}}~//
  %{journal}.~--
  %{yearvolumeissuepages}.~--
  URL: \underline{\smash{\href{%{urlref}}{%{url}}}}
  (access~date: %{accessdate}).
  |}]
;;

let site ~label ~title ~sitename ~url ?(urlref = url) ~accessdate () =
  Done
    [%string
      {|\bibitem{%{label}}
      %{title}~// %{sitename}.~--
      URL: \underline{\smash{\href{%{urlref}}{%{url}}}}
      (дата~обращения: %{accessdate}). |}]
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
      {|\bibitem{rowellherbert}
      Роуэлл,~Г. Физика~: учебное издание~/ Г.~Роуэлл, С.~Герберт.~--
      Москва~: Просвещение, 1994.~--
      576~с.~--
      ISBN 5-09-002920-2.
      |}
  ; Done
      {|\bibitem{alekseevabel}
      Алексеев,~В.~Б. Теорема Абеля в задачах и решениях~/ В.~Б.~Алексеев.~--
      Москва~: МЦНМО, 2001.~--
      192~с.~--
      ISBN 5-900916-86-3.
      |}
  ; Done
      {|\bibitem{bisectionkaw}
      Autar Kaw. Bisection Method of Solving a Nonlinear Equation~: [textbook chapter]~/ Autar K Kaw //
      Textbook: Numerical Methods with Applications.~--
      URL: \underline{\smash{\href{https://nm.mathforcollege.com/mws/gen/03nle/mws\_gen\_nle\_txt\_bisection.pdf}{https://nm.mathforcollege.com/mws/gen/03nle/mws\_gen\_nle\_txt\_bisection.pdf}}}.~--
      Publication date: 15.01.2012.
      |}
  ; Done
      {|\bibitem{wiki-ellastic-collision}
      Elastic collision~// Wikipedia~: the free encyclopedia.~--
      URL: \underline{\smash{\href{https://en.wikipedia.org/wiki/Elastic\_collision}{https://en.wikipedia.org/wiki/Elastic\_collision}}}
      (дата~обращения: 26.05.2022).
      |}
  ; site
      ~label:"mdn-spa"
      ~title:"SPA (Single-page application)"
      ~sitename:"MDN Web Docs"
      ~url:"https://developer.mozilla.org/en-US/docs/Glossary/SPA"
      ~accessdate:"26.05.2022"
      ()
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
  ; Done
      {|\bibitem{emscripten-about}
      About Emscripten~// Emscripten documentation.~--
      URL: \underline{\smash{\href{https://emscripten.org/docs/introducing\_emscripten/about\_emscripten.html}{https://emscripten.org/docs/introducing\_emscripten/about\_emscripten.html}}}
      (дата~обращения: 26.05.2022).
      |}
  ; Done
      {|\bibitem{rust-wasm}
      Why Rust and WebAssembly?~// Rust and WebAssembly~: [документация].~--
      URL: \underline{\smash{\href{https://rustwasm.github.io/docs/book/why-rust-and-webassembly.html}{https://rustwasm.github.io/docs/book/why-rust-and-webassembly.html}}}
      (дата~обращения: 27.05.2022).
    |}
  ; Done
      {|\bibitem{blazor-ru}
      ASP.NET Core Blazor~// ASP.NET Core~: [документация].~--
      URL: \underline{\smash{\href{https://docs.microsoft.com/ru-ru/aspnet/core/blazor}{https://docs.microsoft.com/ru-ru/aspnet/core/blazor}}}
      (дата~обращения: 27.05.2022).
    |}
  ; site
      ~label:"fsbolero"
      ~title:"Getting started"
      ~sitename:"Bolero: F\# in WebAssembly"
      ~url:"https://fsbolero.io/docs/"
      ~accessdate:"27.05.2022"
      ()
  ; site
      ~label:"wasm-iwantto"
      ~title:"I want to..."
      ~sitename:"WebAssembly"
      ~url:"https://webassembly.org/getting-started/developers-guide/"
      ~accessdate:"27.05.2022"
      ()
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
          {|Шутов, В.~С. Функциональное программирование для решения математических задач
    / В. С. Шутов // Фундаментальные исследования основных направлений технических и физико - математических наук
    : сборник статей Международной научно-практической конференции, Челябинск, 01 июня 2018 года.
    – Челябинск: Общество с ограниченной ответственностью "Аэтерна", 2018. – С. 120-122.|}
      ; edn = "XPWRYL"
      }
  ; site
      ~label:"camlhistory"
      ~title:"A History of Caml"
      ~sitename:"The Caml Language"
      ~url:"https://caml.inria.fr/about/history.en.html"
      ~accessdate:"27.05.2022"
      ()
  ; Done
      {|\bibitem{ocamlorg}
      OCaml~: [оффициальный сайт].~--
      URL: \underline{\smash{\href{https://ocaml.org/}{https://ocaml.org/}}}
      (дата~обращения: 27.05.2022).
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
  ; site
      ~label:"opam"
      ~title:"OCaml Package Manager"
      ~sitename:"OPAM"
      ~url:"https://opam.ocaml.org/"
      ~accessdate:"29.05.2022"
      ()
  ; Done
      {|\bibitem{npmjs}
      npm.~--
      URL: \underline{\smash{\href{https://www.npmjs.com/}{https://www.npmjs.com/}}}
      (дата~обращения: 29.05.2022).~--
      Режим доступа: для зарегистрированных пользователей.
      |}
  ; Done
      {|\bibitem{crockford}
      Crockford, D. JavaScript: The World's Most Misunderstood Programming Language~/ Douglas Crockford.~--
      2001.~--
      URL: \underline{\smash{\href{https://www.crockford.com/javascript/javascript.html}{https://www.crockford.com/javascript/javascript.html}}}
      (дата~обращения: 29.05.2022).
      |}
  ; site
      ~label:"ocamljs-lambda"
      ~title:"How ocamljs compiles OCaml to Javascript"
      ~sitename:"[ocamljs~: документация]"
      ~url:"https://jaked.org/ocamljs/Jscomp.html"
      ~accessdate:"29.05.2022"
      ()
  ; rwo2nd
      ~label:"rwo-backend"
      ~title:"The Compiler Backend: Bytecode and Native code"
      ~url:"https://dev.realworldocaml.org/compiler-backend.html"
  ; doi
      ~label:"vouillon-jsoo"
      ~doi:"10.1002/spe.2187"
      ~title:"From bytecode to JavaScript: the Js\_of\_ocaml compiler"
      ~author:"Vouillon J."
      ~authors:"Jérôme Vouillon, Vincent Balat"
      ~journal:"Software: Practice and Experience"
      ~yearvolumeissuepages:"2014.~-- Vol.~44, Iss.~8.~-- P.~951--972"
      ~url:
        "https://www.irif.fr/{\\textasciitilde}balat/publications/vouillon\_balat-js\_of\_ocaml.pdf"
      ~urlref:
        "https://www.irif.fr/\\~balat/publications/vouillon\_balat-js\_of\_ocaml.pdf"
      ~accessdate:"30.05.2022"
  ; site
      ~label:"bobzhang-rawlambda"
      ~title:"Compiling rawlambda output to javascript~: discuss"
      ~sitename:"GitHub~: js\_of\_ocaml"
      ~url:"https://github.com/ocsigen/js\_of\_ocaml/issues/338"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"rescript-introduction"
      ~title:"Introduction"
      ~sitename:"ReScript Language Manual"
      ~url:"https://rescript-lang.org/docs/manual/latest/introduction"
      ~accessdate:"29.05.2022"
      ()
  ; Done
      {|\bibitem{melange}
      Monteiro,~A. On OCaml and the JS platform~/ Antonio Nuno Monteiro.~--
      URL: \underline{\smash{\href{https://anmonteiro.com/2021/03/on-ocaml-and-the-js-platform/}{https://anmonteiro.com/2021/03/on-ocaml-and-the-js-platform/}}}
      (дата~обращения: 29.05.2022).
    |}
  ; Done
      {|\bibitem{ocaml-wasm}
      Chan, V. Run OCaml in the browser by WebAssembly~/ Vincent Chan.~--
      URL: \underline{\smash{\href{https://okcdz.medium.com/run-ocaml-in-the-browser-by-webassembly-31ce464594c6}{https://okcdz.medium.com/run-ocaml-in-the-browser-by-webassembly-31ce464594c6}}}
      (дата~обращения: 29.05.2022).
      |}
  ; site
      ~label:"ocamlverse-libraries"
      ~title:"Standard Libraries"
      ~sitename:"OCamlverse"
      ~url:"https://ocamlverse.github.io/content/standard\_libraries.html"
      ~accessdate:"29.05.2022"
      ()
  ; Done
      {|\bibitem{janestreet-opensource}
      Jane Street Open Source.~--
      URL: \underline{\smash{\href{https://opensource.janestreet.com/}{https://opensource.janestreet.com/}}}
      (дата~обращения: 29.05.2022).
      |}
  ; rwo2nd
      ~label:"rwo-async"
      ~title:"Concurrent Programming with Async"
      ~url:"https://dev.realworldocaml.org/concurrent-programming.html"
  ; doi
      ~label:"vouillon-lwt"
      ~doi:"10.1145/1411304.1411307"
      ~title:"Lwt: a cooperative thread library"
      ~author:"Vouillon, J."
      ~authors:"Jérôme Vouillon"
      ~journal:"ML '08: Proceedings of the 2008 ACM SIGPLAN workshop on ML"
      ~yearvolumeissuepages:"2008.~-- P.~3--12"
      ~url:"https://www.irif.fr//{\\textasciitilde}vouillon/publi/lwt.pdf"
      ~urlref:"https://www.irif.fr//\\~vouillon/publi/lwt.pdf"
      ~accessdate:"28.05.2022"
  ; site
      ~label:"announcing-async"
      ~title:"Announcing Async"
      ~sitename:"Jane Street Tech Blog"
      ~url:"https://blog.janestreet.com/announcing-async/"
      ~accessdate:"28.05.2022"
      ()
  ; Done
      {|\bibitem{rgrinberg-async}
      Grinberg, R. Abandoning Async~/ Rudi Grinberg.~--
      URL: \underline{\smash{\href{http://rgrinberg.com/posts/abandoning-async/}{http://rgrinberg.com/posts/abandoning-async/}}}
      (дата обращения: 27.05.2022).
    |}
  ; site
      ~label:"jsoo-react"
      ~title:"jsoo-react"
      ~sitename:"GitHub"
      ~url:"https://github.com/ml-in-barcelona/jsoo-react"
      ~accessdate:"29.05.2022"
      ()
  ; site
      ~label:"elm-architecture"
      ~title:"The Elm Architecture~: An Introduction to Elm"
      ~sitename:"Elm~: [документация]"
      ~url:"https://guide.elm-lang.org/architecture/"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"lexifi-vdom"
      ~title:"ocaml-vdom"
      ~sitename:"GitHub"
      ~url:"https://github.com/LexiFi/ocaml-vdom"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"janestreet-virtualdom"
      ~title:"virtual\_dom"
      ~sitename:"GitHub"
      ~url:"https://github.com/janestreet/virtual\_dom"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"bonsai-history"
      ~title:"The History of Bonsai.t"
      ~sitename:"Github~: bonsai"
      ~url:"https://github.com/janestreet/bonsai/blob/master/docs/blogs/history.md"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"janestreet-bonsai"
      ~title:"Bonsai"
      ~sitename:"Jane Street Open Source"
      ~url:"https://opensource.janestreet.com/bonsai/"
      ~accessdate:"28.05.2022"
      ()
  ; Done
      {|\bibitem{react-book}
      Бэнкс,~A. React и Redux: функциональная веб-разработка~/ А.~Бэнкс, Е.~Порселло.~--
      Санкт-Петербург~: Питер, 2019.~--
      336~с.~--
      ISBN 978-5-4461-0668-4.
    |}
  ; Done
      {|\bibitem{minsky-incrdom}
      Minsky,~Y. Data Driven UIs, Incrementally~: [доклад]~/ Yaron Minsky~//
      Strange Loop Conference.~--
      2018.~--
      URL: \underline{\smash{\href{https://www.youtube.com/watch?v=R3xX37RGJKE}{https://www.youtube.com/watch?v=R3xX37RGJKE}}}
      (дата~обращения: 27.05.2022).
      |}
  ; site
      ~label:"mdn-websocket"
      ~title:"WebSocket~: Интерфейсы веб API"
      ~sitename:"MDN Web Docs"
      ~url:"https://developer.mozilla.org/ru/docs/Web/API/WebSocket"
      ~accessdate:"28.05.2022"
      ()
  ; Done
      {|\bibitem{dream}
        Dream : Tidy, feature-complete web framework.~--
        URL: \underline{\smash{\href{https://aantron.github.io/dream/}{https://aantron.github.io/dream/}}}
        (дата~обращения: 27.05.2022). |}
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
  ; Done
      {|\bibitem{anil-gemma-qcon}
      Madhavapeddy, A. Fast, Flexible and Functional Programming with OCaml~/
      Anil Madhavapeddy, Gemma Gordon~// QCon Software Development Conference.~--
      London, 2018.~--
      URL: \underline{\smash{\href{https://www.infoq.com/presentations/ocaml-browser-iot/}{https://www.infoq.com/presentations/ocaml-browser-iot/}}}
      (access date: 29.05.2022).
    |}
  ; rwo2nd
      ~label:"rwo-platform"
      ~title:"The OCaml Platform"
      ~url:"https://dev.realworldocaml.org/platform.html"
  ; site
      ~label:"bulma-vs-bootstrap"
      ~title:"An alternative to Bootstrap"
      ~sitename:"Bulma~: [документация]"
      ~url:"https://bulma.io/alternative-to-bootstrap/"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"wiki-mlmodules"
      ~title:"Язык модулей ML"
      ~sitename:"Википедия~: свободная энциклопедия"
      ~url:"https://ru.wikipedia.org/wiki/Язык\_модулей\_ML"
      ~accessdate:"28.05.2022"
      ()
  ; arxiv
      ~label:"functor-driven"
      ~arxiv:"1905.02529"
      ~title:"Programming Unikernels in the Large via Functor Driven Development"
      ~author:"Radanne,~G."
      ~authors:"Gabriel Radanne, Thomas Gazagnaire, Anil Madhavapeddy [et~al.]"
      ~year:"2019"
      ~pubdate:"07.05.2019"
  ; Labeled
      { label = "intftrick"
      ; raw =
          {|Ferguson,~C. The \_intf trick / Craig Ferguson.~--
            URL: \underline{\smash{\href{https://www.craigfe.io/posts/the-intf-trick}{https://www.craigfe.io/posts/the-intf-trick}}}
            (дата~обращения: 27.05.2022).|}
      }
  ; Done
      {|\bibitem{fprog-adt}
      Душкин,~Р. Алгебраические типы данных и их использование в программировании~/
      Р. Душкин~// Практика функционального программирования~: журнал~--
      2009.~--
      выпуск~2.~--
      С.~49--60.~--
      URL: \underline{\smash{\href{https://www.fprog.ru/2009/issue2/practice-fp-2-compact.pdf}{https://www.fprog.ru/2009/issue2/practice-fp-2-compact.pdf}}}
      (дата~обращения: 29.05.2022).
      |}
  ; rwo2nd
      ~label:"rwo-gadt"
      ~title:"GADTs"
      ~url:"https://dev.realworldocaml.org/gadts.html"
  ; site
      ~label:"mdnsvgtag"
      ~title:"<svg>~: SVG element reference"
      ~sitename:"MDN Web Docs"
      ~url:"https://developer.mozilla.org/en-US/docs/Web/SVG/Element/svg"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"mdn-wheel"
      ~title:"Element: wheel event~: Web APIs"
      ~sitename:"MDN Web Docs"
      ~url:"https://developer.mozilla.org/en-US/docs/Web/API/Element/wheel\_event"
      ~accessdate:"28.05.2022"
      ()
  ; site
      ~label:"jsoo-issue-1272"
      ~title:"Wheel event bindings~: feature request"
      ~sitename:"GitHub~: js\_of\_ocaml"
      ~url:"https://github.com/ocsigen/js\_of\_ocaml/issues/1272"
      ~accessdate:"28.05.2022"
      ()
  ; doi
      ~label:"poolpi"
      ~doi:"10.1070/RD2003v008n04ABEH000252"
      ~title:"Playing pool with π (the number π from a billiard point of view)"
      ~author:"Galperin.~G."
      ~authors:"G.~Galperin"
      ~journal:"Regualar and Chaotic Dynamics"
      ~yearvolumeissuepages:"2003.~-- Vol.~8, №~4.~-- P.~375--394"
      ~url:
        "https://www.maths.tcd.ie/{\\textasciitilde}lebed/Galperin. Playing pool with \
         pi.pdf"
      ~urlref:"https://www.maths.tcd.ie/\\~lebed/Galperin. Playing pool with pi.pdf"
      ~accessdate:"25.05.2022"
  ; site
      ~label:"habrpi"
      ~title:"Как увидеть π? Нужно швырнуть π в стену"
      ~sitename:"Habr"
      ~url:"https://habr.com/ru/post/533454/"
      ~accessdate:"29.05.2022"
      ()
  ; Done
      {|\bibitem{browniankrugosvet}
      БРОУНОВСКОЕ ДВИЖЕНИЕ~// Энциклопедия Кругосвет.~--
        URL: \underline{\smash{\href{https://www.krugosvet.ru/enc/nauka\_i\_tehnika/fizika/BROUNOVSKOE\_DVIZHENIE.html}{https://www.krugosvet.ru/enc/nauka\_i\_tehnika/fizika/}}}
        \underline{\smash{\href{https://www.krugosvet.ru/enc/nauka\_i\_tehnika/fizika/BROUNOVSKOE\_DVIZHENIE.html}{BROUNOVSKOE\_DVIZHENIE.html}}}
        (дата~обращения: 30.05.2022). |}
  ; site
      ~label:"wiki-chapaev"
      ~title:"Чапаев (игра)"
      ~sitename:"Википедия~: свободная энциклопедия"
      ~url:"https://ru.wikipedia.org/wiki/Чапаев (игра)"
      ~accessdate:"29.05.2022"
      ()
  ; site
      ~label:"smeshariki-fandom"
      ~title:"Смешарики (игра)"
      ~sitename:"Шарарам вики"
      ~url:"https://shararam.fandom.com/wiki/Смешарики\_(игра)"
      ~accessdate:"29.05.2022"
      ()
  ; site
      ~label:"vk-games"
      ~title:"Игровая платформа"
      ~sitename:"VK для разработчиков"
      ~url:"https://dev.vk.com/games/overview"
      ~accessdate:"29.05.2022"
      ()
  ; site
      ~label:"tg-games"
      ~title:"Gaming Platform"
      ~sitename:"Telegram Bots~: [документация]"
      ~url:"https://core.telegram.org/bots/games"
      ~accessdate:"29.05.2022"
      ()
  ; site
      ~label:"infoqmulticore"
      ~title:"OCaml 5 Will Include Multicore Support"
      ~sitename:"InfoQ"
      ~url:"https://www.infoq.com/news/2021/10/ocaml-5-multicore/"
      ~accessdate:"29.05.2022"
      ()
  ; site
      ~label:"domainslibgithub"
      ~title:"domainslib"
      ~sitename:"GitHub"
      ~url:"https://github.com/ocaml-multicore/domainslib"
      ~accessdate:"29.05.2022"
      ()
  ; Done {|
  
  \end{thebibliography}
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
