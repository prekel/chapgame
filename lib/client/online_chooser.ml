open Core
open Bonsai_web
open Bonsai.Let_syntax

let route : Location.t = [ "online" ], []

let createroom =
  let open Vdom in
  let open Node in
  let open Attr in
  div
    ~attr:(classes [ "box" ])
    [ h5 ~attr:(many [ classes [ "title"; "is-5" ] ]) [ text "Create room" ]
    ; button
        ~attr:
          (many
             [ classes [ "button"; "is-link" ]
             ; on_click (fun _ ->
                   let%bind.Effect id_str =
                     Effect_lwt.of_lwt_unit (fun () ->
                         let%bind.Lwt _response, body =
                           Uri.empty
                           |> (fun uri -> Uri.with_path uri "/api/room/create")
                           |> (fun uri -> Uri.add_query_param' uri ("replay_id", "start"))
                           |> Cohttp_lwt_jsoo.Client.post
                         in
                         Cohttp_lwt.Body.to_string body)
                   in
                   let id, token = id_str |> Sexp.of_string |> [%of_sexp: int * string] in
                   Location.push (Online.route id (Some token)))
             ])
        [ text "Create" ]
    ]
;;

let join =
  let%sub id_field, set_id_field =
    Bonsai.state [%here] (module String) ~default_model:""
  in
  let%sub token_field, set_token_field =
    Bonsai.state [%here] (module String) ~default_model:""
  in
  let%arr id_field = id_field
  and set_id_field = set_id_field
  and token_field = token_field
  and set_token_field = set_token_field in
  let open Vdom in
  let open Node in
  let open Attr in
  div
    ~attr:(classes [ "box" ])
    [ h5 ~attr:(many [ classes [ "title"; "is-5" ] ]) [ text "Join by ID and token" ]
    ; div
        ~attr:(many [ classes [ "field" ] ])
        [ label ~attr:(classes [ "label" ]) [ text "Room ID" ]
        ; div
            ~attr:(classes [ "control" ])
            [ input
                ~attr:
                  (many
                     [ classes [ "input" ]
                     ; type_ "text"
                     ; placeholder ""
                     ; value_prop id_field
                     ; on_input (fun _ -> set_id_field)
                     ])
                []
            ]
        ]
    ; div
        ~attr:(many [ classes [ "field" ] ])
        [ label
            ~attr:(classes [ "label" ])
            [ text "Token (keep empty for only spectating)" ]
        ; div
            ~attr:(classes [ "control" ])
            [ input
                ~attr:
                  (many
                     [ classes [ "input" ]
                     ; type_ "text"
                     ; placeholder "optional"
                     ; value_prop token_field
                     ; on_input (fun _ -> set_token_field)
                     ])
                []
            ]
        ]
    ; div
        ~attr:(many [ classes [ "field" ] ])
        [ div
            ~attr:(classes [ "control" ])
            [ button
                ~attr:
                  (many
                     [ classes [ "button"; "is-link" ]
                     ; on_click (fun _ ->
                           match Int.of_string id_field, token_field with
                           | exception _ -> Effect.Ignore
                           | id, "" -> Location.push (Online.route id None)
                           | id, token -> Location.push (Online.route id (Some token)))
                     ])
                [ text "Join" ]
            ]
        ]
    ]
;;

let component =
  let%sub join = join in
  let%arr join = join in
  let open Vdom in
  let open Node in
  let open Attr in
  div
    [ createroom
    ; h4 ~attr:(many [ classes [ "title"; "is-4"; "centertext" ] ]) [ text "or" ]
    ; join
    ]
;;
