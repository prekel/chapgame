open Core
module C = (val Chapgame.Utils.make_consts ~eps:1e-6)
module S = Chapgame.Scene.Make (C)

module Room =
  Room.Make (C) (S)
    (struct
      let replay _ = assert false
    end)

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
           [ Dream.get "/client_bin.bc.js" (fun _ ->
                 loader "client_bin.bc.js" ~content_type:`application_javascript)
           ; Dream.get "/**" (fun _ -> loader "index.html" ~content_type:`text_html)
           ]
       ]
;;
