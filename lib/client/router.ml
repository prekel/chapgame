open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module SC = Scene.Make (Defaults.C) (Defaults.S)
module Offline = Offline.Make (Defaults.C) (Defaults.S) (SC) (Defaults.Replays)
module Online = Online.Make (Defaults.C) (Defaults.S)

let fake_router =
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
  match%sub tab with
  | `Offline ->
    let%sub tab_click =
      let%arr set_tab = set_tab in
      function
      | `Offline -> Effect.Ignore
      | `Online -> set_tab `Online
    in
    let%sub inner, outer = Offline.component in
    Bar.component ~inner ~outer ~opened_tab:`Offline ~tab_click
  | `Online ->
    let%sub tab_click =
      let%arr set_tab = set_tab in
      function
      | `Offline -> set_tab `Offline
      | `Online -> Effect.Ignore
    in
    Bar.component
      ~inner:(Bonsai.Value.return Error_pages.tbd)
      ~outer:(Bonsai.Value.return Vdom.Node.none)
      ~opened_tab:`Online
      ~tab_click
;;

let router =
  let%sub location = Location.use () in
  match%sub location with
  | [ "" ], _ ->
    let%sub inner, outer = Offline.component in
    Bar.component
      ~inner
      ~outer
      ~opened_tab:`Offline
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Effect.Ignore
            | `Online -> Location.push (Online.route 1 None)))
  | [ "online"; room_id ], token ->
    let%sub inner, outer =
      Online.component
        ~room_id
        ~token:
          (Bonsai.Value.map token ~f:(fun token ->
               List.Assoc.find ~equal:String.equal token "token"))
    in
    Bar.component
      ~inner
      ~outer
      ~opened_tab:`Online
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Location.push Offline.route
            | `Online -> Effect.Ignore))
  | _ ->
    Bar.component
      ~inner:(Bonsai.Value.return Error_pages.not_found)
      ~outer:(Bonsai.Value.return Vdom.Node.none)
      ~opened_tab:`Not_found
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Location.push Offline.route
            | `Online -> Location.push (Online.route 1 None)))
;;

let component =
  if Dom_html.document##.cookie
     |> Js.to_string
     |> String.is_substring ~substring:(fst Protocol.Cookie.chapgame_online_supported)
  then router
  else fake_router
;;
