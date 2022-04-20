open Core
open Lwt.Let_syntax
module SC = Chapgame.Scene.Make (Float) ((val Chapgame.Utils.make_consts ~eps:1e-6))

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

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Dream.html "")
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
                 let model, diff =
                   SC.Engine.recv_with_diff room.model ~action:(`Action action)
                 in
                 Hashtbl.update rooms room_id ~f:(function
                     | Some prev -> { prev with model }
                     | _ -> assert false);
                 DreamExt.sexp [%sexp (diff : SC.Model.Diff.diff)])
           ; Dream.get "/:room_id/ws" (fun request ->
                 let _room_id = Dream.param request "room_id" in
                 Dream.websocket (fun client ->
                     let rec loop () =
                       match%bind Dream.receive client with
                       | Some _message -> loop ()
                       | None -> Dream.close_websocket client
                     in
                     loop ()))
           ]
       ]
;;
