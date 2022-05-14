open Bonsai_web
open Bonsai.Let_syntax

type tab =
  [ `Game
  | `Offline
  | `Online
  ]

(* opened_tab:tab Value.t -> show_tabs:bool Value.t -> is_tab_enabled:(tab -> bool)
   Value.t -> inner:Vdom.Node.t Computation.t -> outer:Vdom.Node.t Computation.t ->
   Vdom.Node.t Computation.t *)
(* ~opened_tab ~show_tabs ~is_tab_enabled *)

let component ~inner ~outer =
  let%sub inner = inner in
  let%sub outer = outer in
  let%arr inner = inner
  and outer = outer in
  let open Vdom in
  let open Node in
  let open Attr in
  div
    ~attr:(many [ classes [ "columns" ] ])
    [ div
        ~attr:(many [ classes [ "column"; "is-narrow" ] ])
        [ Node.div
            ~attr:(many [ style (Css_gen.width (`Px_float 300.)) ])
            [ Node.create
                "nav"
                ~attr:(many [ classes [ "navbar" ] ])
                [ div
                    ~attr:(many [ classes [ "navbar-menu" ] ])
                    [ div
                        ~attr:(many [ classes [ "navbar-start" ] ])
                        [ div ~attr:(many [ classes [ "navbar-item" ] ]) [ text "1" ]
                        ; div ~attr:(many [ classes [ "navbar-item" ] ]) [ text "2" ]
                        ; div ~attr:(many [ classes [ "navbar-item" ] ]) [ text "3" ]
                        ]
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
