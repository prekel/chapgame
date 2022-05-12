open Core
open Js_of_ocaml

type t =
  { websocket : (WebSockets.webSocket Js.t[@sexp.opaque])
  ; url : string
  ; stream : (string Lwt_stream.t[@sexp.opaque])
  }
[@@deriving sexp]

let equal = phys_equal

let connect url =
  let url = Uri.to_string url in
  let websocket = new%js WebSockets.webSocket (Js.string url) in
  let stream, push = Lwt_stream.create () in
  websocket##.onmessage
    := Dom.handler (fun (event : _ WebSockets.messageEvent Js.t) ->
           let data = Js.to_string event##.data in
           push (Some data);
           Js._false);
  websocket##.onclose
    := Dom.handler (fun (event : _ WebSockets.closeEvent Js.t) ->
           let _ = event##.code in
           push None;
           Js._false);
  { websocket; url; stream }
;;

let send { websocket; _ } ~msg = websocket##send (Js.string msg)
let close { websocket; _ } = websocket##close
let stream { stream; _ } = stream
