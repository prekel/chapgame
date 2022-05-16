open Bonsai_web
open Bonsai.Let_syntax

type tab =
  [ `Offline
  | `Online
  ]

(* opened_tab:tab Value.t -> show_tabs:bool Value.t -> is_tab_enabled:(tab -> bool)
   Value.t -> inner:Vdom.Node.t Computation.t -> outer:Vdom.Node.t Computation.t ->
   Vdom.Node.t Computation.t *)
(* ~opened_tab ~show_tabs ~is_tab_enabled *)

let tab_text = function
  | `Offline -> "Offline"
  | `Online -> "Online"
;;

let component ~inner ~outer ~opened_tab ~tab_click =
  let%sub inner = inner in let%sub outer = outer in
  let%arr inner = inner
  and outer = outer
  and opened_tab = opened_tab
  and tab_click = tab_click in
  let open Vdom in
  let open Node in
  let open Attr in
  div
    ~attr:(many [ classes [ "columns" ] ])
    [ div
        ~attr:(many [ classes [ "column"; "is-narrow"; "bar-column" ] ])
        [ Node.div
            ~attr:(many [ style (Css_gen.min_width (`Px 300)) ])
            [ Node.create
                "nav"
                ~attr:(many [ classes [ "navbar" ] ])
                [ div
                    ~attr:(many [ classes [ "navbar-brand" ] ])
                    [ a ~attr:(many [ classes [ "navbar-item" ] ]) [ text "chapgame" ] ]
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
            ; Node.div
                ~attr:(many [ classes [ "container"; "is-fluid"; "is-primary" ] ])
                [ inner ]
            ]
        ]
    ; div ~attr:(many [ classes [ "column" ] ]) [ outer ]
    ]
;;
