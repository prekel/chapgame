(* open Core *)

let content =
  {|\begingroup
\renewcommand{\section}[2]{\Anonchapter{Список использованных источников}\vspace{-1em}}
\begin{thebibliography}{00}

    \bibitem{kant}
    Критика чистого разума
    Сочинения в шести томах.
    Том 3
    Академия наук СССР
    Институт философии
    Издательство социально-экономической литературы
    «Мысль»
    Москва - 1964. \TODO

    \bibitem{rowellherbert}
    Роуэлл, Г. Физика : учебное издание / Г. Роуэлл, С. Герберт. -- Москва : Просвещение, 1994. -- 576 с. -- ISBN 5-09-002920-2.

    \bibitem{mathforprogrammers}
    Math for Progammers \TODO
    https://www.manning.com/books/math-for-programmers

    \bibitem{larson}
    Larson R., Hostetler R. . Precalculus: A Concise Course. — Boston:
    Houghton Mifflin, 2007. — xvii + 526 + 102 p. — ISBN 0-618-62719-7. \TODO

    \bibitem{alekseevabel}
    Алексеев, В. Б. Теорема Абеля в задачах и решениях. — М.: МЦНМО, 2001. — 192 с. — ISBN 5-900916-86-3. \TODO

    \bibitem{wiki-ellastic-collision}
    https://en.wikipedia.org/wiki/Elastic\_collision \TODO

    \bibitem{bisectionkaw}
    \TODO Autar K Kaw Numerical Methods with Applications Chapter 03.03 Bisection Method

    \bibitem{mdn-spa}
    https://developer.mozilla.org/en-US/docs/Glossary/SPA \TODO

    \bibitem{wasm}
    Andreas Haas, Andreas Rossberg, Derek L. Schuff, Ben L. Titzer, Michael Holman, Dan Gohman, Luke Wagner, Alon Zakai, and JF Bastien.
    2017. Bringing the web up to speed with WebAssembly.
    In Proceedings of the 38th ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI 2017).
    Association for Computing Machinery, New York, NY, USA, 185–200. https://doi.org/10.1145/3062341.3062363 \TODO

    \bibitem{emscripten-about}
    https://emscripten.org/docs/introducing\_emscripten/about\_emscripten.html \TODO

    \bibitem{rust-wasm}
    https://rustwasm.github.io/docs/book/why-rust-and-webassembly.html \TODO

    \bibitem{blazor-ru}
    https://docs.microsoft.com/ru-ru/aspnet/core/blazor \TODO

    \bibitem{fsbolero}
    https://fsbolero.io/docs/ \TODO

    \bibitem{wasm-iwantto}
    https://webassembly.org/getting-started/developers-guide/ \TODO

    \bibitem{dsyme-hopl}
    https://fsharp.org/history/hopl-final/hopl-fsharp.pdf
    Don Syme. 2020. The early history of F\#. Proc. ACM Program. Lang. 4, HOPL, Article 75 (June 2020), 58 pages. https://doi.org/10.1145/3386325
    \TODO

    \bibitem{typescript-mayorov}
    Майоров, А. TypeScript для PHP-разработчика. Как писать на JavaScript большие приложения и не сойти с ума /
    А. Майоров // Системный администратор. – 2015. – № 7-8(152-153). – С. 95-99. – EDN UBPAGH.

    \bibitem{shutov-haskell}
    Шутов, В. С. Функциональное программирование для решения математических задач
    / В. С. Шутов // Фундаментальные исследования основных направлений технических и физико - математических наук
    : сборник статей Международной научно-практической конференции, Челябинск, 01 июня 2018 года.
    – Челябинск: Общество с ограниченной ответственностью "Аэтерна", 2018. – С. 120-122. – EDN XPWRYL.

    \bibitem{camlhistory}
    https://caml.inria.fr/about/history.en.html \TODO

    \bibitem{ocamlorg}
    https://ocaml.org/ \TODO

    \bibitem{yaron2011}
    Yaron Minsky. OCaml for the Masses. ACM Queue, Sep 27, 2011 \TODO

    \bibitem{rwo-prologue}
    Real World OCaml. 2nd Edition. Yaron Minsky. Anil Madhavapeddy.
    https://dev.realworldocaml.org/prologue.html \TODO

    \bibitem{opam}
    https://opam.ocaml.org/ \TODO

    \bibitem{npmjs}
    https://www.npmjs.com/ \TODO Режим доступа: для зарегистрированных пользователей.

    \bibitem{crockford}
    https://www.crockford.com/javascript/javascript.html \TODO

    \bibitem{ocamljs-lambda}
    https://jaked.org/ocamljs/Jscomp.html \TODO

    \bibitem{rwo-backend}
    https://dev.realworldocaml.org/compiler-backend.html \TODO

    \bibitem{vouillon-jsoo}
    https://www.irif.fr/~balat/publications/vouillon\_balat-js\_of\_ocaml.pdf \TODO

    \bibitem{bobzhang-rawlambda}
    https://github.com/ocsigen/js\_of\_ocaml/issues/338 \TODO

    \bibitem{rescript-introduction}
    https://rescript-lang.org/docs/manual/latest/introduction \TODO

    \bibitem{melange}
    https://anmonteiro.com/2021/03/on-ocaml-and-the-js-platform/ \TODO

    \bibitem{ocaml-wasm}
    https://okcdz.medium.com/run-ocaml-in-the-browser-by-webassembly-31ce464594c6 \TODO

    \bibitem{ocamlverse-libraries}
    https://ocamlverse.github.io/content/standard\_libraries.html \TODO

    \bibitem{janestreet-opensource}
    https://opensource.janestreet.com/ \TODO

    \bibitem{rwo-ru}
    Мински Я., Мадхавапедди А., Хикки Дж.
    М57 Программирование на языке OCaml / пер. с анг.л А. Н. Киселева. –
    М.: ДМК Пресс, 2014. – 536 с.: ил.
    ISBN 978-5-97060-102-0
    \TODO

    \bibitem{vouillon-lwt}
    Jerome Vouillon Lwt: a Cooperative Thread Library \TODO
    https://www.irif.fr/\~vouillon/publi/lwt.pdf

    \bibitem{announcing-async}
    https://blog.janestreet.com/announcing-async/ \TODO

    \bibitem{rgrinberg-async}
    http://rgrinberg.com/posts/abandoning-async/ \TODO

    \bibitem{jsoo-react}
    https://github.com/ml-in-barcelona/jsoo-react \TODO

    \bibitem{elm-architecture}
    https://guide.elm-lang.org/architecture/ \TODO

    \bibitem{lexifi-vdom}
    https://github.com/LexiFi/ocaml-vdom \TODO

    \bibitem{janestreet-virtualdom}
    https://github.com/janestreet/virtual\_dom \TODO

    \bibitem{bonsai-history}
    https://github.com/janestreet/bonsai/blob/master/docs/blogs/history.md \TODO

    \bibitem{janestreet-bonsai}
    https://opensource.janestreet.com/bonsai/ \TODO

    \bibitem{react-book}
    А. Бэнкс, Е. Порселло. React и Redux: функциональная веб-разработка ISBN 978-5-4461-0668-4 \TODO

    \bibitem{minsky-incrdom}
    https://www.youtube.com/watch?v=R3xX37RGJKE \TODO

    \bibitem{mdn-websocket}
    https://developer.mozilla.org/ru/docs/Web/API/WebSocket \TODO

    \bibitem{dream}
    https://aantron.github.io/dream \TODO

    \bibitem{rwo-testing}
    https://dev.realworldocaml.org/testing.html

    \bibitem{rwo-runtime-memory}
    https://dev.realworldocaml.org/runtime-memory-layout.html \TODO

    \bibitem{rwo-json}
    https://dev.realworldocaml.org/json.html \TODO

    \bibitem{rwo-sexp}
    https://dev.realworldocaml.org/data-serialization.html \TODO

    \bibitem{anil-gemma-qcon}
    https://www.infoq.com/presentations/ocaml-browser-iot/ \TODO

    \bibitem{rwo-platform}
    https://dev.realworldocaml.org/platform.html \TODO

    \bibitem{bulma-vs-bootstrap}
    https://bulma.io/alternative-to-bootstrap/ \TODO

    \bibitem{wiki-mlmodules}
    https://ru.wikipedia.org/wiki/Язык\_модулей\_ML

    \bibitem{functor-driven}
    https://arxiv.org/abs/1905.02529 Programming Unikernels in the Large via Functor Driven
    Development (Experience Report) \TODO Radanne G. et al. Programming unikernels in the large via functor driven development //arXiv preprint arXiv:1905.02529. – 2019.

    \bibitem{fprog-adt}
    https://www.fprog.ru/2009/issue2/practice-fp-2-compact.pdf 
    Алгебраические типы данных и их использование в программировании с. 49
    Роман Душкин \TODO

\end{thebibliography}
\endgroup
|}
;;

let () =
  Stdio.Out_channel.with_file
    ~fail_if_exists:false
    "c2-bibliography.tex"
    ~f:Stdio.Out_channel.(fun c -> output_string c content)
;;
