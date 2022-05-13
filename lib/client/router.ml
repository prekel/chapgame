(* open Core *)
open Bonsai_web
open Bonsai.Let_syntax
module C = (val Chapgame.Utils.make_consts ~eps:1e-6)
module S = Chapgame.Scene.Make (C)
module SC = Scene.Make (C) (S)
module Offline = Offline.Make (C) (S) (SC)

let main_page =
  Bonsai.const
    Vdom.(Node.button ~attr:(Attr.on_click (fun _ -> Location.push Offline.route)) [])
;;

let room room_id token =
  let%arr room_id = room_id
  and token = token in
  Vdom.(
    Node.div
      [ Node.text room_id
      ; (match token with
        | Some token -> Node.text token
        | None -> Node.none)
      ])
;;

let not_found = Bonsai.const (Vdom.Node.text "not found")

let component =
  let%sub location = Location.use () in
  match%sub location with
  | [ "" ], _ -> main_page
  | [ "room"; room_id ], [ ("token", token) ] ->
    let token =
      let%map token = token in
      Some token
    in
    room room_id token
  | [ "room"; room_id ], _ -> room room_id (Bonsai.Value.return None)
  | [ "offline" ], _ -> Offline.component
  | _ -> not_found
;;
