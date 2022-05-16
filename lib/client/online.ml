open Core
open Bonsai_web
open Bonsai.Let_syntax

module Make
    (C : Engine.Module_types.CONSTS with module N = Float)
    (S : module type of Engine.Scene.Make (C)) =
    struct
  let route room_id token : Location.t =
    ( [ "online"; Int.to_string room_id ]
    , match token with
      | Some token -> [ "token", token ]
      | None -> [] )
  ;;

  let component ~room_id ~token =
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
end
