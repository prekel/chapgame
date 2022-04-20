open Core
open Lwt.Let_syntax
module P = Chapgame.Protocol.Make (Float) ((val Chapgame.Utils.make_consts ~eps:1e-6))
module SC = P.SC

module Client = struct
  module Id = Chapgame.Utils.MakeIntId (struct
    let module_name = "Client.Id"
  end)

  type t = Dream.websocket
end

module Clients = struct
  type t = (Client.Id.t, Client.t) Hashtbl.t
end

module Room = struct
  module Id = Chapgame.Utils.MakeIntId (struct
    let module_name = "Room.Id"
  end)

  type t =
    { model : SC.Model.t
    ; clients : Clients.t
    }

  let init () =
    { model = SC.Model.init ~g:10.; clients = Hashtbl.create (module Client.Id) }
  ;;
end

module Rooms = struct
  type t = (Room.Id.t, Room.t) Hashtbl.t

  let init () = Hashtbl.create (module Room.Id)
end

let rooms_field : Rooms.t Dream.field = Dream.new_field ()
let rooms_var = Rooms.init ()

let rooms_middleware inner_handler request =
  Dream.set_field request rooms_field rooms_var;
  inner_handler request
;;

module DreamExt = struct
  let sexp ?status ?code ?headers ?mach sexp =
    let body =
      match mach with
      | Some true -> Sexp.to_string_mach sexp
      | _ -> Sexp.to_string_hum sexp
    in
    let response = Dream.response ?status ?code ?headers body in
    Dream.set_header response "Content-Type" "text/plain";
    Lwt.return response
  ;;
end

let room_of_request request =
  let rooms = Dream.field request rooms_field |> Option.value_exn in
  let room_id = Dream.param request "room_id" |> Room.Id.of_string in
  rooms, Hashtbl.find_exn rooms room_id, room_id
;;

let update_room ~(rooms : Rooms.t) ~(room : Room.t) ~room_id action =
  let model, diff = SC.Engine.recv_with_diff room.model ~action:(`Action action) in
  Hashtbl.update rooms room_id ~f:(function
      | Some prev -> { prev with model }
      | _ -> assert false);
  let%map _ =
    Lwt_list.iter_p
      (fun (_id, client) ->
        Dream.send
          client
          ([%sexp (P.Response.Diff diff : P.Response.t)] |> Sexp.to_string_hum))
      (room.clients |> Hashtbl.to_alist)
  in
  diff
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Dream.html Embedded_files.index_dot_html)
       ; Dream.get "/main.bc.js" (fun _ ->
             Dream.respond Embedded_files.main_dot_bc_dot_js)
       ; Dream.scope
           "/room"
           [ rooms_middleware ]
           [ Dream.post "/create" (fun request ->
                 let rooms = Dream.field request rooms_field |> Option.value_exn in
                 let id = Room.Id.next () in
                 Hashtbl.add_exn rooms ~key:id ~data:(Room.init ());
                 DreamExt.sexp [%sexp (id : Room.Id.t)])
           ; Dream.get "/:room_id" (fun request ->
                 let _, room, _ = room_of_request request in
                 DreamExt.sexp [%sexp (room.model : SC.Model.t)])
           ; Dream.post "/:room_id/action" (fun request ->
                 let rooms, room, room_id = room_of_request request in
                 let%bind body = Dream.body request in
                 let action = body |> Sexp.of_string |> [%of_sexp: SC.Action.t] in
                 let%bind diff = update_room action ~room ~rooms ~room_id in
                 DreamExt.sexp [%sexp (diff : SC.Model.Diff.diff)])
           ; Dream.get "/:room_id/ws" (fun request ->
                 Dream.websocket (fun client ->
                     let client_id = Client.Id.next () in
                     let _, room, _ = room_of_request request in
                     Hashtbl.add_exn room.clients ~key:client_id ~data:client;
                     let rec loop () =
                       let rooms, room, room_id = room_of_request request in
                       match%bind Dream.receive client with
                       | Some message ->
                         let (Action action) =
                           message |> Sexp.of_string |> [%of_sexp: P.Request.t]
                         in
                         let%bind _diff = update_room action ~room ~rooms ~room_id in
                         loop ()
                       | None ->
                         Hashtbl.remove room.clients client_id;
                         Dream.close_websocket client
                     in
                     loop ()))
           ]
       ]
;;
