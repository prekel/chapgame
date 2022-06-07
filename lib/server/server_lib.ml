open Core
open Lwt.Let_syntax

(* module Room =
  Room.Make (Defaults.C) (Defaults.S)
    (struct
      let replay _ = assert false
    end) *)

let loader path ~content_type =
  match Assets.read path with
  | None -> Dream.empty `Not_Found
  | Some asset ->
    Dream.respond
      asset
      ~headers:
        [ ( "Content-Type"
          , match content_type with
            | `text_html -> "text/html; charset=utf-8"
            | `application_javascript -> "application/javascript; charset=utf-8" )
        ]
;;

let main () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [ Dream.scope "/api" [] [ Room.route ]
       ; Dream.scope
           "/"
           [ Dream_encoding.compress ]
           [ Dream.get "/client_bin.bc.js" (fun _ -> Dream.empty `No_Content)
           ; Dream.get "/assets/client_bin.bc.js" (fun _ ->
                 loader "client_bin.bc.js" ~content_type:`application_javascript)
           ; Dream.get "/**" (fun request ->
                 let%map response = loader "index.html" ~content_type:`text_html in
                 Dream.set_cookie
                   ~http_only:false
                   ~encrypt:false
                   response
                   request
                   (fst Protocol.Cookie.chapgame_online_supported)
                   (snd Protocol.Cookie.chapgame_online_supported);
                 response)
           ]
       ]
;;
