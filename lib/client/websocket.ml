open Core
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

let connect' url ~on_message ~on_close =
  let url = Uri.to_string url in
  let websocket = new%js WebSockets.webSocket (Js.string url) in
  websocket##.onmessage
    := Dom.handler (fun (event : _ WebSockets.messageEvent Js.t) ->
           let data = Js.to_string event##.data in
           on_message data;
           Js._false);
  websocket##.onclose
    := Dom.handler (fun (event : _ WebSockets.closeEvent Js.t) ->
           let _ = event##.code in
           on_close ();
           Js._false);
  websocket
;;

let use url =
  let last_message_var = Bonsai.Var.create None in
  let%sub initer =
    let%arr url = url in
    fun _ ->
      connect'
        url
        ~on_message:(fun msg -> Bonsai.Var.set last_message_var (Some msg))
        ~on_close:Fn.id
  in
  let%sub websocket, _ =
    Bonsai_extra.state_dynamic_model
      [%here]
      (module struct
        type t = WebSockets.webSocket Js.t

        let sexp_of_t _ = Sexp.List []
        let t_of_sexp _ = assert false
        let equal = phys_equal
      end)
      ~model:(`Computed initer)
  in
  let%arr websocket = websocket
  and last_message = Bonsai.Var.value last_message_var in
  let send_message msg =
    Effect.of_sync_fun (fun msg -> websocket##send msg) (Js.string msg)
  in
  send_message, last_message
;;
