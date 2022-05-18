open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

let tab_text = function
  | `Offline -> "Offline"
  | `Online -> "Online"
;;

let component ~inner ~outer ~opened_tab ~tab_click =
  (* let%sub inner = inner in let%sub outer = outer in *)
  let%arr inner = inner
  and outer = outer
  and opened_tab = opened_tab
  and tab_click = tab_click in
  let open Vdom in
  let open Node in
  let open Attr in
  div
    ~attr:(many [ classes [ "columns"; "is-gapless" ] ])
    [ div
        ~attr:(many [ classes [ "column"; "is-narrow"; "bar-column" ] ])
        [ Node.div
            ~attr:(many [])
            [ Node.create
                "nav"
                ~attr:(many [ classes [ "navbar" ] ])
                [ div
                    ~attr:(many [ classes [ "navbar-brand" ] ])
                    [ a
                        ~attr:
                          (many
                             [ classes [ "navbar-item" ]
                             ; on_click (fun _ ->
                                   Dom_html.window##.location##reload;
                                   Effect.Ignore)
                             ])
                        [ text "chapgame" ]
                    ]
                ; div
                    ~attr:(many [ classes [ "navbar-menu" ] ])
                    [ div
                        ~attr:(many [ classes [ "navbar-end" ] ])
                        [ a
                            ~attr:
                              (many
                                 [ classes [ "navbar-item" ]
                                 ; href "https://github.com/prekel/chapgame"
                                 ])
                            [ span
                                ~attr:(class_ "icon")
                                [ Node.create
                                    "i"
                                    ~attr:(classes [ "fab"; "fa-github" ])
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ; div
                ~attr:(many [ classes [ "tabs"; "is-centered" ] ])
                [ ul
                    [ li
                        ~attr:
                          (many
                             [ (match opened_tab with
                               | `Offline -> class_ "is-active"
                               | _ -> empty)
                             ; on_click (fun _ -> tab_click `Offline)
                             ])
                        [ a [ Node.text (tab_text `Offline) ] ]
                    ; li
                        ~attr:
                          (many
                             [ (match opened_tab with
                               | `Online -> class_ "is-active"
                               | _ -> empty)
                             ; on_click (fun _ -> tab_click `Online)
                             ])
                        [ a [ Node.text (tab_text `Online) ] ]
                    ]
                ]
            ; div
                ~attr:(many [ classes [ "scroll-y" ] ])
                [ div ~attr:(many [ classes [ "directionltr" ] ]) [ inner ] ]
            ]
        ]
    ; div ~attr:(many [ classes [ "column" ] ]) [ outer ]
    ]
;;
