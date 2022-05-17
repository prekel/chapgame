open Core
open Bonsai_web
open Bonsai.Let_syntax
module SC = Scene.Make (Defaults.C) (Defaults.S)
module Offline = Offline.Make (Defaults.C) (Defaults.S) (SC) (Defaults.Replays)
module Online = Online.Make (Defaults.C) (Defaults.S)

let main_page =
  Bonsai.const
    Vdom.(Node.button ~attr:(Attr.on_click (fun _ -> Location.push Offline.route)) [])
;;

let not_found =
  Bonsai.const
    Vdom.(
      Node.button
        ~attr:(Attr.on_click (fun _ -> Location.back ()))
        [ Node.text "not_found" ])
;;

let fake_router =
  let%sub a, b = Offline.component in
  let%sub tab, set_tab =
    Bonsai.state
      [%here]
      (module struct
        type t =
          [ `Offline
          | `Online
          ]
        [@@deriving sexp, equal]
      end)
      ~default_model:`Offline
  in
  let%sub tab_click_offline =
    let%arr set_tab = set_tab in
    function
    | `Offline -> Effect.Ignore
    | `Online -> set_tab `Online
  in
  let%sub tab_click_online =
    let%arr set_tab = set_tab in
    function
    | `Offline -> set_tab `Offline
    | `Online -> Effect.Ignore
  in
  match%sub tab with
  | `Offline ->
    Bar.component
      ~inner:a
      ~outer:b
      ~opened_tab:(Bonsai.Value.return `Offline)
      ~tab_click:tab_click_offline
  | `Online ->
    let%sub not_found = not_found in
    Bar.component
      ~inner:not_found
      ~outer:b
      ~opened_tab:(Bonsai.Value.return `Online)
      ~tab_click:tab_click_online
;;

let router =
  let%sub location = Location.use () in
  match%sub location with
  | [ "online"; room_id ], _ ->
    let%sub c = Online.component ~room_id ~token:(Bonsai.Value.return None) in
    Bar.component
      ~inner:c
      ~outer:c
      ~opened_tab:(Bonsai.Value.return `Online)
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Location.push Offline.route
            | `Online -> Effect.Ignore))
  | [ "" ], _ ->
    let%sub a, b = Offline.component in
    Bar.component
      ~inner:a
      ~outer:b
      ~opened_tab:(Bonsai.Value.return `Offline)
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Effect.Ignore
            | `Online -> Location.push (Online.route 1 (Some "qwfqwf"))))
  | _ -> not_found
;;

open Js_of_ocaml

let component =
  if Dom_html.document##.cookie
     |> Js.to_string
     |> String.is_substring ~substring:(fst Protocol.Cookie.chapgame_online_supported)
  then router
  else fake_router
;;
