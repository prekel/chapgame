(* open Core *)
open Bonsai_web
open Bonsai.Let_syntax
module SC = Scene.Make (Defaults.C) (Defaults.S)
module Offline = Offline.Make (Defaults.C) (Defaults.S) (SC) (Defaults.Replays)
module Room = Room.Make (Defaults.C) (Defaults.S)

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
  match%sub location with
  | [ "" ], _ -> main_page
  | [ "room"; room_id ], [ ("token", token) ] ->
    Room.component ~room_id ~token:(Bonsai.Value.map ~f:Option.some token)
  | [ "room"; room_id ], _ -> Room.component ~room_id ~token:(Bonsai.Value.return None)
  | [ "offline" ], _ -> Offline.component
  | _ -> not_found
;;
