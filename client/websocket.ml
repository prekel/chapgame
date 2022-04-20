open Core
open Js_of_ocaml

type t =
  { websocket : (WebSockets.webSocket Js.t[@sexp.opaque])
  ; url : string
  }
[@@deriving sexp]

let equal = Poly.equal

let connect url ~on_message =
  let url = Uri.to_string url in
  let websocket = new%js WebSockets.webSocket (Js.string url) in
  websocket##.onmessage
    := Dom.handler (fun (event : _ WebSockets.messageEvent Js.t) ->
           let data = Js.to_string event##.data in
           on_message data;
           Js._false);
  { websocket; url }
;;

let send { websocket; _ } data = websocket##send (Js.string data)
let close { websocket; _ } = websocket##close
