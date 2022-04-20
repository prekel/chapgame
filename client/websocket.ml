open Js_of_ocaml

type t = WebSockets.webSocket Js.t

let connect url ~var =
  match new%js WebSockets.webSocket (Js.string (Uri.to_string url)) with
  | exception Js.Error exn -> failwith (Js.to_string exn##.message)
  | exception exn -> raise exn
  | websocket ->
    (* let onclose (_close_event : _ WebSockets.closeEvent Js.t) = Js._false in *)
    let onmessage (event : _ WebSockets.messageEvent Js.t) =
      let data = Js.to_string event##.data in
      Bonsai.Var.update var ~f:(fun prev -> data :: prev);
      Js._false
    in
    (* websocket##.onerror := Dom.handler (fun (_ : _ Dom.event Js.t) -> Js._false); *)
    websocket##.onmessage := Dom.handler onmessage;
    (* websocket##.onclose := Dom.handler onclose; *)
    websocket
;;

let send (websocket : WebSockets.webSocket Js.t) data = websocket##send (Js.string data)
