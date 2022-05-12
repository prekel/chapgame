open Core
open Lwt.Let_syntax

module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) (Payload : sig
      include Sexpable.S
      include Equal.S with type t := t
    end) =
    struct
  module N = C.N
  module Protocol = Engine.Protocol.Make1 (C) (S) (Payload)

  module Client = struct
    module Id = Common.Utils.MakeIntId (struct
      let module_name = "Client.Id"
    end)

    type t = { websocket : Dream.websocket }
  end

  module Clients = struct
    type t = (Client.Id.t, Client.t) Hashtbl.t
  end

  module Room = struct
    module Id = Common.Utils.MakeIntId (struct
      let module_name = "Room.Id"
    end)

    type t =
      { model : S.Model.t
      ; clients : (Clients.t[@sexp.opaque])
      ; lock : (Lwt_mutex.t[@sexp.opaque])
      ; time : N.t
      ; speed : N.t
      ; payload : Payload.t
      ; token : (string[@sexp.opaque])
      }
    [@@deriving sexp]

    let to_response ?diff (room : t) =
      Protocol.Response.
        { time = room.time
        ; speed = room.speed
        ; payload = room.payload
        ; diff =
            (match diff with
            | Some diff -> `Diff diff
            | None -> `Replace room.model)
        }
    ;;

    let update_room room Protocol.Request.{ time; speed; action } =
      Lwt_mutex.with_lock room.lock (fun () ->
          let%map model, diff =
            match action with
            | `Action _ as a ->
              let%map model, diff =
                Lwt_preemptive.detach
                  (fun (model, action) -> S.Engine.recv_with_diff model ~action)
                  (room.model, a)
              in
              model, Some diff
            | `Replace model -> Lwt.return (model, None)
          in
          { room with model; time; speed }, diff)
    ;;
  end

  module Rooms = struct
    type t = (Room.Id.t, Room.t) Hashtbl.t

    let init () = Hashtbl.create (module Room.Id)
  end

  let rooms_field : Rooms.t Dream.field = Dream.new_field ()
  let rooms_var = Rooms.init ()

  let middleware inner_handler request =
    Dream.set_field request rooms_field rooms_var;
    inner_handler request
  ;;

  let room_of_request request =
    let rooms = Dream.field request rooms_field |> Option.value_exn in
    let room_id = Dream.param request "room_id" |> Room.Id.of_string in
    rooms, Hashtbl.find_exn rooms room_id, room_id
  ;;

  let broadcast_response lock clients response =
    Lwt_mutex.with_lock lock (fun () ->
        let clients = Hashtbl.to_alist clients in
        let message = response |> [%sexp_of: Protocol.Response.t] |> Sexp.to_string_hum in
        Lwt_list.iter_p
          (fun (_id, Client.{ websocket }) -> Dream.send websocket message)
          clients)
  ;;

  let route =
    Dream.scope
      "/room"
      [ middleware ]
      [ Dream.get "/:room_id" (fun request ->
            let _, room, _ = room_of_request request in
            Dream_ext.sexp [%sexp (room : Room.t)])
      ; Dream.get "/:room_id/model" (fun request ->
            let _, room, _ = room_of_request request in
            Dream_ext.sexp [%sexp (room.model : S.Model.t)])
      ; Dream.post "/:room_id/action" (fun request ->
            let rooms, room, room_id = room_of_request request in
            let token = Dream.query request "token" in
            match token with
            | Some token when String.(token = room.token) ->
              let%bind body = Dream.body request in
              let r = body |> Sexp.of_string |> [%of_sexp: Protocol.Request.t] in
              let () =
                Lwt.async (fun () ->
                    let%bind new_room, diff = Room.update_room room r in
                    Hashtbl.update rooms room_id ~f:(fun _ -> new_room);
                    let response = Room.to_response ?diff new_room in
                    broadcast_response room.lock room.clients response)
              in
              Dream.empty `Accepted
            | _ -> Dream.empty `Forbidden)
      ; Dream.get "/:room_id/ws" (fun request ->
            Dream.websocket (fun client ->
                let client_id = Client.Id.next () in
                let _, room, _ = room_of_request request in
                let%bind () =
                  Room.to_response room
                  |> [%sexp_of: Protocol.Response.t]
                  |> Sexp.to_string_hum
                  |> Dream.send client
                in
                let rec loop () =
                  let%bind msg = Dream.receive client in
                  match msg with
                  | Some _ -> loop ()
                  | None ->
                    Lwt_mutex.with_lock room.lock (fun () ->
                        Hashtbl.remove room.clients client_id;
                        Dream.close_websocket client)
                in
                loop ()))
      ]
  ;;
end