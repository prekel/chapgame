(* open Core *)
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

let component =
  let%sub location = Location.use () in
  let%sub a, b = Offline.component in
  match%sub location with
  | [ "online"; room_id ], _ ->
    let c = Online.component ~room_id ~token:(Bonsai.Value.return None) in
    Bar.component
      ~inner:c
      ~outer:c
      ~opened_tab:(Bonsai.Value.return `Online)
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Location.push Offline.route
            | `Online -> Effect.Ignore))
  | [ "" ], _ ->
    Bar.component
      ~inner:(Bonsai.read a)
      ~outer:(Bonsai.read b)
      ~opened_tab:(Bonsai.Value.return `Offline)
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Effect.Ignore
            | `Online -> Location.push (Online.route 1 (Some "qwfqwf"))))
  | [ "chapgame" ], _ ->
    (* Gh pages *)
    Bar.component
      ~inner:(Bonsai.read a)
      ~outer:(Bonsai.read b)
      ~opened_tab:(Bonsai.Value.return `Offline)
      ~tab_click:
        (Bonsai.Value.return (function
            | `Offline -> Effect.Ignore
            | `Online -> Effect.Ignore))
  | _ -> not_found
;;
