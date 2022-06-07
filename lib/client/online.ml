open Core
open Bonsai_web
open Bonsai.Let_syntax

let route room_id token : Location.t =
  ( [ "online"; Int.to_string room_id ]
  , match token with
    | Some token -> [ "token", token ]
    | None -> [] )
;;

let component ~room_id ~token =
  let%arr _room_id = room_id
  and _token = token in
  let open Vdom in
  let open Node in
  (* let open Attr in *)
  let inner = Error_pages.tbd in
  let outer = none in
  inner, outer
;;
